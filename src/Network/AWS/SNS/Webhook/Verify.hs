{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.AWS.SNS.Webhook.Verify (
  HasDownloadSNSCertificate (..)
, MonadVerify
, VerificationError (..)
, CertificateCache
, verifyMessage

-- * Utilities to implement 'downloadSNSCertificate' and default implementations
, parseAndVerifySNSCertificate
, downloadSNSCertificateDefault
, downloadSNSCertificateWithCache
, withCertCache
, embeddedCertificateStore

-- * Re-exports
, SignatureVerification(..)
, ValidationCache
, CertificateStore
, readCertificateStore
, exceptionValidationCache
, tofuValidationCache
) where


import           Network.AWS.SNS.Webhook.Types (Message, Signature (..),
                                                SignedText (..), Url (..),
                                                messageSignature,
                                                messageSignedText,
                                                messageSigningCertURL)


import           Control.Exception.Safe        (Exception, SomeException,
                                                bracket, catch)
import           Control.Lens                  (view, (^.))
import           Control.Monad                 (forever)
import           Control.Monad.Catch           (MonadCatch, MonadMask)
import           Control.Monad.Error.Lens      (throwing)
import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Control.Monad.Reader          (MonadReader)
import           Crypto.Store.X509             (readSignedObjectFromMemory)
import           Data.ByteString               (ByteString)
import qualified Data.Cache                    as Cache
import           Data.FileEmbed                (embedFile)
import           Data.Generics.Product         as X (HasType (..))
import           Data.Generics.Sum             as X (AsType (..))
import           Data.String.Conv              (toS)
import qualified Data.Text                     as T
import           Data.Time                     (NominalDiffTime)
import           Data.X509                     (Certificate (certPubKey),
                                                CertificateChain (..),
                                                HashALG (HashSHA1),
                                                SignatureALG (SignatureALG),
                                                getCertificate, pubkeyToAlg)
import           Data.X509.CertificateStore    (CertificateStore,
                                                makeCertificateStore,
                                                readCertificateStore)
import           Data.X509.Validation          (FailedReason (..),
                                                SignatureVerification (..),
                                                ValidationCache,
                                                exceptionValidationCache,
                                                tofuValidationCache,
                                                validateDefault,
                                                verifySignature)
import           GHC.Generics                  (Generic)
import           Network.HTTP.Client           (Manager, httpLbs,
                                                requestFromURI, responseBody)
import           Network.URI                   (uriAuthority, uriRegName)
import           UnliftIO.Async                (async, cancel)
import           UnliftIO.Concurrent           (threadDelay)


data VerificationError
  = NoCertificates
  | InvalidCertificate [FailedReason] CertificateChain
  | CertificateDowloadError SomeException
  | InvalidSigningCertUrl
  deriving (Show, Generic, Exception)

type MonadVerify m r e =
  ( MonadReader r m
  , HasType CertificateStore r
  , HasType ValidationCache r
  , MonadError e m
  , AsType VerificationError e
  , MonadIO m
  )

class HasDownloadSNSCertificate m where
  -- | Download a 'Certificate' from the 'SigningCertUrl'.
  --
  -- Implementations *must* validate the certificate and may cache it (keyed on
  -- 'Url') to avoid repeated network access and verification
  downloadSNSCertificate :: Url -> m Certificate

-- | Verifies the signature of a 'Message' using the 'HasDownloadSNSCertificate'
-- implemenatition to produce a verified certificate from the Url
verifyMessage
  :: ( MonadVerify m r e
     , HasDownloadSNSCertificate m
     )
  => Message
  -> m SignatureVerification
verifyMessage message = do
  cert <- downloadSNSCertificate (message ^. messageSigningCertURL)
  pure (verifyMessageWithCert cert message)


-- | Parse and verify a certificate in PEM format.
--
-- Useful to implement 'downloadSNSCertificate'
parseAndVerifySNSCertificate
  :: MonadVerify m r e
  => ByteString
  -> m Certificate
parseAndVerifySNSCertificate s = do
  (cert, chain) <- case readSignedObjectFromMemory s of
    []              -> throwing _Typed NoCertificates
    certs@(scert:_) -> pure (getCertificate scert, CertificateChain certs)
  store <- view typed
  cache <- view typed
  errors <- liftIO $ validateDefault store cache ("sns.amazonaws.com","") chain
  case errors of
    [] -> pure cert
    _  -> throwing _Typed $ InvalidCertificate errors chain


-- | Verifies the signature of a 'Message' using a given 'Certificate'. It is
-- the responsibility of the caller to verify the 'Certificate' is valid
verifyMessageWithCert :: Certificate -> Message -> SignatureVerification
verifyMessageWithCert cert msg = verify (msg^.messageSignedText) (msg^.messageSignature)
  where
  pubKey = certPubKey cert
  alg = SignatureALG HashSHA1 (pubkeyToAlg pubKey)
  verify (SignedText signed) (Signature signature) =
    verifySignature alg pubKey signed (toS signature)

-- | Downloads and verifies a 'Certificate'.
--
-- A 'VerificationError' will be thrown through 'MonadError' if we fail to
-- download or verify the certificate
downloadSNSCertificateDefault
  :: ( MonadCatch m
     , MonadVerify m r e
     , HasType Manager r
     )
  => [T.Text]
  -> Url
  -> m Certificate
downloadSNSCertificateDefault allowedCertHostnameSuffixes (Url url)
  | Just auth <- uriAuthority url
  , any (`T.isSuffixOf` T.pack (uriRegName auth)) allowedCertHostnameSuffixes = do
  req <- maybe (throwing _Typed InvalidSigningCertUrl) pure (requestFromURI url)
  mgr <- view typed
  resp <- liftIO (httpLbs req mgr) `catch` (throwing _Typed . CertificateDowloadError)
  parseAndVerifySNSCertificate (toS $ responseBody resp)
downloadSNSCertificateDefault _ _ = throwing _Typed InvalidSigningCertUrl


--  | An in-memory cache for verified certificates.
--
--  In-memory and per-node (vs, eg, redis) is preferred here because:
--
--    * There are very few (most probably just one) certificates we need to
--      cache and they are long lived (months or years)
--
--    * We can store a parsed and verified certificate instead of the serialized
--      certificate and avoid deserializing it whenever we bring it from external
--      storage
type CertificateCache = Cache.Cache Url Certificate

-- | Downloads and verifies a 'Certificate' and memoizes the result in a
-- 'CertificateCache'.
downloadSNSCertificateWithCache
  :: ( MonadCatch m
     , MonadVerify m r e
     , HasType Manager r
     , HasType CertificateCache r
     )
  => [T.Text]
  -> Url
  -> m Certificate
downloadSNSCertificateWithCache allowedCertHostnameSuffixes surl = do
  cache <- view typed
  -- We use lookup' to avoid purging the element if it has been expired. If it
  -- has, a Nothing will be returned so it will get overwritten with a fresh
  -- version anyway.
  mCert <- liftIO (Cache.lookup' cache surl)
  case mCert of
    Just cert -> pure cert
    Nothing -> do
      cert <- downloadSNSCertificateDefault allowedCertHostnameSuffixes surl
      liftIO (Cache.insert cache surl cert)
      pure cert

-- Initializes a 'CertificateCache' with a default TTL and passes it to the
-- continuation.
--
-- A reaper thread will be started to purge expired entries every minute which
-- will be cancelled when the continuation is done.
--
-- Note that no CRL will be checked while the Certificate is in the cache (for
-- performance) so it is recommended to initialize the 'CertificateCache' with
-- an appropiate default TTL to reduce the window a revoked certificate could
-- sign and send us naughty data.
withCertCache
  :: (MonadIO m, MonadMask m)
  => NominalDiffTime
  -> (CertificateCache -> m a)
  -> m a
withCertCache expireTime f = bracket initialize destroy (f . fst)
  where

  initialize = liftIO $ do
    cache <- Cache.newCache (Just (fromIntegral (toNanoSeconds expireTime)))
    reaper <- async $ forever $ do
      threadDelay (60 * 1000000) -- Purge expired every minute
      Cache.purgeExpired cache
    pure (cache, reaper)

  destroy (_, reaper) = cancel reaper

  toNanoSeconds :: NominalDiffTime -> Int
  toNanoSeconds = floor . (*1e9)

-- | An embedded 'CertificateStore' which contains
-- https://www.amazontrust.com/repository/R1-ServerCA1B.pem
--
-- Valid until Oct 19 00:00:00 2025 GMT
embeddedCertificateStore :: CertificateStore
embeddedCertificateStore =
  makeCertificateStore $
    readSignedObjectFromMemory $(embedFile "R1-ServerCA1B.pem")
