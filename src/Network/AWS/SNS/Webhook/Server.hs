{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Network.AWS.SNS.Webhook.Server (
  MonadSNSWebhook
, SnsWebhookApi
, Blank
, webhookServer
) where

import           Network.AWS.SNS.Webhook.Types
import           Network.AWS.SNS.Webhook.Verify

import           Control.Lens
import           Control.Monad                 (void)
import           Control.Monad.Error.Lens      (throwing)
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Control.Monad.Logger          (MonadLogger, logDebugN, logErrorN,
                                                logInfoN)
import           Data.Aeson                    (FromJSON)
import           Data.Generics.Product         as X (HasType (..))
import           Data.Generics.Sum.Typed       (AsType (..))
import           Data.Monoid                   ((<>))
import           Data.Proxy                    (Proxy (Proxy))
import           Data.String.Conv              (toS)
import           Data.Text                     (Text)
import           Network.HTTP.Client           (Manager, httpNoBody,
                                                parseUrlThrow, setQueryString)
import           Servant
import           Servant.API.ContentTypes      (AllCTRender (handleAcceptH))

type SnsWebhookApi a =
  ReqBody '[Blank] (Message a) :> Post '[Blank] ()

type MonadSNSWebhook m r e =
  ( HasType Manager r
  , MonadVerify m r e
  , AsType ServantErr e
  , MonadLogger m
  , HasDownloadSNSCertificate m
  )

webhookServer
  :: (Show a, MonadSNSWebhook m r e)
  => (a -> m ())
  -> ServerT (SnsWebhookApi a) m
webhookServer onMessage msg = do
  throwIfUnverifiable msg
  case msg of

    MsgSubscriptionConfirmation _ Confirmation{topicArn,token,subscribeURL} -> do
      logDebugN $ "Received SubscriptionConfirmation  to " <> topicArn
      confirmSubscription subscribeURL token topicArn
      logInfoN $ "Confirmed subscription to " <> topicArn

    MsgNotification _ Notification{message} -> onMessage message

    MsgUnsubscribeConfirmation _ Confirmation{topicArn} ->
      logInfoN $ "Received UnsubscribeConfirmation to " <> topicArn

{-# INLINEABLE webhookServer #-}

throwIfUnverifiable
  :: (Show a, MonadSNSWebhook m r e) => Message a -> m ()
throwIfUnverifiable msg = do
  verification <- verifyMessage msg
  case verification of
    SignaturePass -> pure ()
    SignatureFailed failure -> do
      logErrorN $ "Could not verify SNS notification: "
        <> toS (show failure)
        <> ": " <> toS (show msg)
      throwing _Typed err403

confirmSubscription
  :: MonadSNSWebhook m r e
  => Text -> Text -> Text -> m ()
confirmSubscription subscribeUrl token topicArn = do
  let mReq = parseUrlThrow (toS subscribeUrl)
         <&> setQueryString
             [ ("Action",   Just "ConfirmSubscription")
             , ("Token",    Just (toS token))
             , ("TopicArn", Just (toS topicArn))
             ]
  case mReq of
    Nothing -> throwing _Typed err400 { errBody = "Invalid subscribeURL" }
    Just req -> do
      manager <- view typed
      void $ liftIO $ httpNoBody req manager

-- We need this because AWS sends blank Accept and Content-Type headers
data Blank

instance Accept Blank where
  contentType _ = "*/*"

instance AllCTRender '[Blank] () where
  handleAcceptH _ _ _ = Just ("text/plain","")

instance FromJSON a => MimeUnrender Blank a where
  mimeUnrender _ = mimeUnrender (Proxy @JSON)
