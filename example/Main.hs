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
import           Data.Maybe                           (fromMaybe)
import           Network.HTTP.Client.TLS              (newTlsManager)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           System.Environment                   (lookupEnv)

main :: IO ()
main = withCertCache 300 $ \certCache -> do
  certStorePath <- fromMaybe "/etc/ssl/certs/ca-bundle.crt"
    <$> lookupEnv "SSL_CERT_FILE"
  port <- maybe 3000 read <$> lookupEnv "PORT"
  mCertStore <- readCertificateStore certStorePath
  certStore <- case mCertStore of
    Just x  -> pure x
    Nothing -> fail $ "Could not read certificates at " <> certStorePath
  manager <- newTlsManager
  validationCache <- tofuValidationCache []
  let mkServer = webhookServer certCache certStore validationCache manager
  putStrLn $ "Running example webhook at port " <> show port
  run port $ logStdoutDev $ serve (Proxy @SnsWebhookApi) $ mkServer $ \notif ->
    liftIO $ print notif
