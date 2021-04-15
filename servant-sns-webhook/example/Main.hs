{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (isJust)
import Data.Text (Text)
import Network.AWS.SNS.Webhook
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (run)
import Servant (Proxy (..), serve)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = withCertCache 300 $ \certCache -> do
  port <- maybe 3000 read <$> lookupEnv "PORT"
  manager <- newTlsManager

  mCertStorePath <- lookupEnv "SSL_CERT_FILE"
  certStore <- case mCertStorePath of
    Nothing -> pure embeddedCertificateStore
    Just path -> do
      mStore <- readCertificateStore path
      maybe (fail $ "Could not read certificates at " <> path) pure mStore

  let mkServer = webhookServer certStore certCache manager

  hPutStrLn stderr $ "Running webhook at port " <> show port

  doShowJsonMessage <- isJust <$> lookupEnv "SHOW_JSON_MESSAGE"
  if doShowJsonMessage
    then run port $
      serve (Proxy @(SnsWebhookApi (Embedded Value))) $
        mkServer $ \Notification {message} ->
          liftIO $ LBS.putStrLn $ encode $ unEmbedded message
    else run port $
      serve (Proxy @(SnsWebhookApi Text)) $
        mkServer $ \notification ->
          liftIO $ LBS.putStrLn $ encode notification
