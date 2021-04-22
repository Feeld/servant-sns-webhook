{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Network.AWS.SNS.Webhook.Server
  ( MonadSNSWebhook,
    ConfirmSubscription (..),
    SnsWebhookApi,
    webhookServerT,
    webhookServer,
    confirmSubscriptionHttpClient,
  )
where

import Control.Exception.Safe (SomeException, catch)
import Control.Lens (view, (<&>))
import Control.Monad (void, (<=<))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger
  ( LoggingT,
    MonadLogger,
    logDebugN,
    logErrorN,
    logInfoN,
    runStderrLoggingT,
  )
import Control.Monad.Reader
  ( MonadReader,
    ReaderT (runReaderT),
  )
import Data.Aeson (FromJSON)
import Data.Generics.Product (HasType (typed))
import Data.Generics.Sum.Typed (AsType (_Typed))
import Data.Proxy (Proxy (Proxy))
import Data.String.Conv (toS)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.AWS.SNS.Webhook.Types
import Network.AWS.SNS.Webhook.Verify
import Network.HTTP.Client
  ( Manager,
    httpNoBody,
    requestFromURI,
    setQueryString,
  )
import Servant
  ( Accept (..),
    Handler,
    JSON,
    MimeUnrender (..),
    NoContent (..),
    PlainText,
    PostNoContent,
    ReqBody,
    ServerError (errBody),
    err400,
    err403,
    err500,
    err504,
    hoistServer,
    runHandler,
    (:>),
  )
import Servant.API.ContentTypes (AllCTRender (handleAcceptH))
import System.IO (hPrint, stderr)

type SnsWebhookApi a =
  ReqBody '[Blank, JSON] (Message a) :> PostNoContent '[Blank, JSON, PlainText] NoContent

type MonadSNSWebhook m r e =
  ( AsType ServerError e,
    MonadError e m,
    AsType VerificationError e,
    MonadLogger m,
    HasDownloadSNSCertificate m,
    ConfirmSubscription m,
    MonadCatch m
  )

data WebhookEnv = WebhookEnv
  { certStore :: CertificateStore,
    certCache :: CertificateCache,
    validationCache :: ValidationCache,
    manager :: Manager
  }
  deriving (Generic)

data WebhookError
  = WebhookServerError ServerError
  | WebhookVerificationError VerificationError
  deriving (Show, Generic)

webhookServer ::
  forall a.
  ( Show a,
    FromJSON a
  ) =>
  CertificateStore ->
  CertificateCache ->
  Manager ->
  (Notification a -> Handler ()) ->
  Message a ->
  Handler NoContent
webhookServer certStore certCache manager onNotification =
  hoistServer
    (Proxy @(SnsWebhookApi a))
    (runWebhookServer WebhookEnv {certStore, validationCache, manager, certCache})
    (webhookServerT (either (throwing _Typed) pure <=< liftIO . runHandler . onNotification))
  where
    -- We don't need a functioning validationCache because the
    -- 'HasDownloadSNSCertificate WebhookHandler' instance caches the certificate
    -- once successfully verified.
    validationCache = exceptionValidationCache []

type WebhookHandler = LoggingT (ReaderT WebhookEnv (ExceptT WebhookError Handler))

instance HasDownloadSNSCertificate WebhookHandler where
  -- The implementation of 'downloadSNSCertificateWithCache' for the Handler
  -- monad that 'webhookServer' uses only downloads certificates from domains
  -- which belong to amazonaws and caches them
  downloadSNSCertificate = downloadSNSCertificateWithCache [".amazonaws.com"]

instance ConfirmSubscription WebhookHandler where
  confirmSubscription = confirmSubscriptionHttpClient

runWebhookServer :: WebhookEnv -> WebhookHandler a -> Handler a
runWebhookServer env hdlr = do
  eRet <- runExceptT $ runReaderT (runStderrLoggingT hdlr) env
  case eRet of
    Left e@(WebhookServerError err) -> logE e >> throwError err
    Left e@(WebhookVerificationError NoCertificates) -> logE e >> throwError badConfig
    Left e@(WebhookVerificationError (InvalidCertificate _ _)) -> logE e >> throwError err403
    Left e@(WebhookVerificationError InvalidSigningCertUrl) -> logE e >> throwError err403
    Left e@(WebhookVerificationError (CertificateDowloadError _)) -> logE e >> throwError err504
    Right a -> pure a
  where
    badConfig = err500 {errBody = "Bad configuration"}
    logE = liftIO . hPrint stderr

webhookServerT ::
  ( Show a,
    MonadSNSWebhook m r e
  ) =>
  (Notification a -> m ()) ->
  Message a ->
  m NoContent
webhookServerT onNotification msg = do
  throwIfUnverifiable msg
  case msg of
    MsgSubscriptionConfirmation _ Confirmation {topicArn, token, subscribeURL} -> do
      logDebugN $ "Received SubscriptionConfirmation to " <> topicArn
      confirmSubscription subscribeURL token topicArn
      logInfoN $ "Confirmed subscription to " <> topicArn
      pure NoContent
    MsgNotification _ notification -> do
      logDebugN $ "Received notification " <> toS (show notification)
      onNotification notification >> pure NoContent
    MsgUnsubscribeConfirmation _ Confirmation {topicArn} -> do
      logInfoN $ "Received UnsubscribeConfirmation to " <> topicArn
      pure NoContent
{-# INLINEABLE webhookServerT #-}

throwIfUnverifiable ::
  ( Show a,
    MonadSNSWebhook m r e
  ) =>
  Message a ->
  m ()
throwIfUnverifiable msg = do
  verification <- verifyMessage msg
  case verification of
    SignaturePass -> pure ()
    SignatureFailed failure -> do
      logErrorN $
        "Could not verify SNS notification: "
          <> toS (show failure)
          <> ": "
          <> toS (show msg)
      throwing _Typed err403

class ConfirmSubscription m where
  confirmSubscription :: Url -> Text -> Text -> m ()

confirmSubscriptionHttpClient ::
  ( MonadError e m,
    AsType ServerError e,
    MonadReader r m,
    HasType Manager r,
    MonadIO m,
    MonadCatch m
  ) =>
  Url ->
  Text ->
  Text ->
  m ()
confirmSubscriptionHttpClient (Url subscribeUrl) token topicArn = do
  let mReq =
        requestFromURI subscribeUrl
          <&> setQueryString
            [ ("Action", Just "ConfirmSubscription"),
              ("Token", Just (toS token)),
              ("TopicArn", Just (toS topicArn))
            ]
  case mReq of
    Nothing -> throwing _Typed err400 {errBody = "Invalid subscribeURL"}
    Just req -> do
      manager <- view typed
      liftIO (void $ httpNoBody req manager)
        `catch` (\(_ :: SomeException) -> throwing _Typed err504)

-- We need this because AWS sends blank Accept and Content-Type headers
data Blank

instance Accept Blank where
  contentType _ = "*/*"

instance AllCTRender '[Blank] NoContent where
  handleAcceptH _ _ _ = Just ("text/plain", "")

instance FromJSON a => MimeUnrender Blank a where
  mimeUnrender _ = mimeUnrender (Proxy @JSON)
