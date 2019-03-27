{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
module Network.AWS.SNS.Webhook.Types (
  Message(..)
, Confirmation(..)
, Notification(..)

, SigningCertUrl (..)
, SignedText (..)
, Signature (..)
, messageSigningCertURL
, messageSignature
, messageSignedText
) where

import           Control.Lens
import           Data.Aeson
import           Data.List (foldl')
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Base64  as B64
import           Data.ByteString.Builder (byteString, shortByteString,
                                          toLazyByteString)
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.String.Conv        (toS)
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           GHC.Generics            (Generic)

newtype SignedText = SignedText ByteString
  deriving (Eq, Show)

newtype Signature = Signature ByteString
  deriving (Eq, Show)

instance ToJSON Signature where
  toJSON (Signature s) = String $ toS $ B64.encode s

instance FromJSON Signature where
  parseJSON = withText "Signature" $ \s ->
    either fail (pure . Signature) $ B64.decode (toS s)


newtype SigningCertUrl = SigningCertUrl Text
  deriving stock Generic
  deriving newtype (Eq, Show, Ord, Hashable, ToJSON, FromJSON)

data Message a
  = MsgNotification             SignedText (Notification a)
  | MsgSubscriptionConfirmation SignedText Confirmation
  | MsgUnsubscribeConfirmation  SignedText Confirmation
  deriving (Eq, Show)

messageSigningCertURL :: Lens' (Message a) SigningCertUrl
messageSigningCertURL = lens getIt setIt
  where
  getIt (MsgNotification _ Notification{signingCertURL}) = signingCertURL
  getIt (MsgSubscriptionConfirmation _ Confirmation{signingCertURL}) = signingCertURL
  getIt (MsgUnsubscribeConfirmation _ Confirmation{signingCertURL}) = signingCertURL

  setIt (MsgNotification st notif) v = MsgNotification st notif { signingCertURL = v}
  setIt (MsgSubscriptionConfirmation st conf) v = MsgSubscriptionConfirmation st conf { signingCertURL = v }
  setIt (MsgUnsubscribeConfirmation st conf)  v = MsgUnsubscribeConfirmation st conf { signingCertURL = v }
{-# INLINE messageSigningCertURL #-}

messageSignature :: Lens' (Message a) Signature
messageSignature = lens getIt setIt
  where
  getIt (MsgNotification _ Notification{signature})             = signature
  getIt (MsgSubscriptionConfirmation _ Confirmation{signature}) = signature
  getIt (MsgUnsubscribeConfirmation _ Confirmation{signature})  = signature

  setIt (MsgNotification st notif) v = MsgNotification st notif { signature = v}
  setIt (MsgSubscriptionConfirmation st conf) v = MsgSubscriptionConfirmation st conf { signature = v }
  setIt (MsgUnsubscribeConfirmation st conf)  v = MsgUnsubscribeConfirmation st conf { signature = v }
{-# INLINE messageSignature #-}

messageSignedText :: Lens' (Message a) SignedText
messageSignedText = lens getIt setIt
  where
  getIt (MsgNotification st _)             = st
  getIt (MsgSubscriptionConfirmation st _) = st
  getIt (MsgUnsubscribeConfirmation st _)  = st

  setIt (MsgNotification _ notif) st = MsgNotification st notif
  setIt (MsgSubscriptionConfirmation _ conf) st = MsgSubscriptionConfirmation st conf
  setIt (MsgUnsubscribeConfirmation _ conf)  st = MsgUnsubscribeConfirmation st conf
{-# INLINE messageSignedText #-}

data Confirmation = Confirmation
  { messageId        :: Text
  , topicArn         :: Text
  , token            :: Text
  , subscribeURL     :: Text
  , message          :: Text
  , timestamp        :: UTCTime
  , signatureVersion :: Text
  , signature        :: Signature
  , signingCertURL   :: SigningCertUrl
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Notification a = Notification
  { messageId        :: Text
  , topicArn         :: Text
  , subject          :: Maybe Text
  , unsubscribeURL   :: Text
  , message          :: a
  , timestamp        :: UTCTime
  , signatureVersion :: Text
  , signature        :: Signature
  , signingCertURL   :: SigningCertUrl
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromJSON a => FromJSON (Message a) where
  parseJSON = withObject "SNS Message" $ \o -> do
    type_ <- o .: "Type"
    case type_ :: Text of
      "Notification" ->
        MsgNotification
          <$> pure (notificationSignedText o)
          <*> genericParseJSON defaultOptions (Object o)
      "SubscriptionConfirmation" ->
        MsgSubscriptionConfirmation
          <$> pure (confirmationSignedText o)
          <*> genericParseJSON defaultOptions (Object o)
      "UnsubscribeConfirmation" ->
        MsgUnsubscribeConfirmation
          <$> pure (confirmationSignedText o)
          <*> genericParseJSON defaultOptions (Object o)
      _ -> fail "Unknown Type"

notificationSignedText :: Object -> SignedText
notificationSignedText = signedTextParser
  [ "Message"
  , "MessageId"
  , "Subject"
  , "SubscribeURL"
  , "Timestamp"
  , "TopicArn"
  , "Type"
  ]

confirmationSignedText :: Object -> SignedText
confirmationSignedText = signedTextParser
  [ "Message"
  , "MessageId"
  , "Subject"
  , "SubscribeURL"
  , "Timestamp"
  , "Token"
  , "TopicArn"
  , "Type"
  ]

signedTextParser :: [ByteString] -> Object -> SignedText
signedTextParser keys o = SignedText . toS . toLazyByteString $ foldl' go mempty keys
  where
  go !acc k = (acc <>) $ maybe mempty (kvLine k) (toS k `HM.lookup` o)
  kvLine k (String v) =
       byteString k
    <> shortByteString "\n"
    <> byteString (toS v)
    <> shortByteString "\n"
  kvLine _ _ = mempty
