{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Main (main) where

import           Network.AWS.SNS.Webhook

import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           (encode)
import qualified Data.ByteString.Lazy.Char8           as LBS
import           Network.HTTP.Client.TLS              (newTlsManager)
import           Network.Wai.Handler.Warp             (run)
import           Servant                              (Proxy (..), serve)
import           System.Environment                   (lookupEnv)
import           System.IO                            (hPutStrLn, stderr)

main :: IO ()
main = withCertCache 300 $ \certCache -> do
  port <- maybe 3000 read <$> lookupEnv "PORT"
  manager <- newTlsManager

  validationCache <- tofuValidationCache []

  mCertStorePath <- lookupEnv "SSL_CERT_FILE"
  certStore <- case mCertStorePath of
    Nothing -> pure embeddedCertificateStore
    Just path -> do
      mStore <- readCertificateStore path
      maybe (fail $ "Could not read certificates at " <> path) pure mStore

  let mkServer = webhookServer certStore certCache validationCache manager

  hPutStrLn stderr $ "Running webhook at port " <> show port
  run port $ serve (Proxy @SnsWebhookApi) $ mkServer $ \notification ->
    liftIO $ LBS.putStrLn (encode notification)
