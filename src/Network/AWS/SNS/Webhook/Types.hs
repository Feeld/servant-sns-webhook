{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
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
, notificationJSON
) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens              (_JSON)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Base64       as B64
import           Data.ByteString.Builder      (byteString, shortByteString,
                                               toLazyByteString)
import           Data.Char                    (toUpper)
import           Data.Generics.Product.Fields (field)
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict          as HM
import           Data.List                    (foldl')
import           Data.String.Conv             (toS)
import           Data.Text                    (Text)
import           Data.Time                    (UTCTime)
import           GHC.Generics                 (Generic)

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

data Message
  = MsgNotification             SignedText Notification
  | MsgSubscriptionConfirmation SignedText Confirmation
  | MsgUnsubscribeConfirmation  SignedText Confirmation
  deriving (Eq, Show)

messageSigningCertURL :: Lens' Message SigningCertUrl
messageSigningCertURL = lens getIt setIt
  where
  getIt (MsgNotification _ Notification{signingCertURL}) = signingCertURL
  getIt (MsgSubscriptionConfirmation _ Confirmation{signingCertURL}) = signingCertURL
  getIt (MsgUnsubscribeConfirmation _ Confirmation{signingCertURL}) = signingCertURL

  setIt (MsgNotification st notif) v = MsgNotification st notif { signingCertURL = v}
  setIt (MsgSubscriptionConfirmation st conf) v = MsgSubscriptionConfirmation st conf { signingCertURL = v }
  setIt (MsgUnsubscribeConfirmation st conf)  v = MsgUnsubscribeConfirmation st conf { signingCertURL = v }
{-# INLINE messageSigningCertURL #-}

messageSignature :: Lens' Message Signature
messageSignature = lens getIt setIt
  where
  getIt (MsgNotification _ Notification{signature})             = signature
  getIt (MsgSubscriptionConfirmation _ Confirmation{signature}) = signature
  getIt (MsgUnsubscribeConfirmation _ Confirmation{signature})  = signature

  setIt (MsgNotification st notif) v = MsgNotification st notif { signature = v}
  setIt (MsgSubscriptionConfirmation st conf) v = MsgSubscriptionConfirmation st conf { signature = v }
  setIt (MsgUnsubscribeConfirmation st conf)  v = MsgUnsubscribeConfirmation st conf { signature = v }
{-# INLINE messageSignature #-}

messageSignedText :: Lens' Message SignedText
messageSignedText = lens getIt setIt
  where
  getIt (MsgNotification st _)             = st
  getIt (MsgSubscriptionConfirmation st _) = st
  getIt (MsgUnsubscribeConfirmation st _)  = st

  setIt (MsgNotification _ notif) st = MsgNotification st notif
  setIt (MsgSubscriptionConfirmation _ conf) st = MsgSubscriptionConfirmation st conf
  setIt (MsgUnsubscribeConfirmation _ conf)  st = MsgUnsubscribeConfirmation st conf
{-# INLINE messageSignedText #-}

notificationJSON :: (FromJSON a, ToJSON a) => Traversal' Notification a
notificationJSON = field @"message" . _JSON
{-# INLINE notificationJSON #-}

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
  } deriving (Eq, Show, Generic)

instance FromJSON Confirmation where
  parseJSON = genericParseJSON jsonOpts

data Notification = Notification
  { messageId        :: Text
  , topicArn         :: Text
  , subject          :: Maybe Text
  , unsubscribeURL   :: Text
  , message          :: Text
  , timestamp        :: UTCTime
  , signatureVersion :: Text
  , signature        :: Signature
  , signingCertURL   :: SigningCertUrl
  } deriving (Eq, Show, Generic)


instance FromJSON Notification where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON Notification where
  toJSON = injectType "Notification"
         . genericToJSON jsonOpts

instance FromJSON Message where
  parseJSON = withObject "SNS Message" $ \o -> do
    type_ <- o .: "Type"
    case type_ :: Text of
      "Notification" ->
        MsgNotification
          <$> pure (notificationSignedText o)
          <*> parseJSON (Object o)
      "SubscriptionConfirmation" ->
        MsgSubscriptionConfirmation
          <$> pure (confirmationSignedText o)
          <*> parseJSON (Object o)
      "UnsubscribeConfirmation" ->
        MsgUnsubscribeConfirmation
          <$> pure (confirmationSignedText o)
          <*> parseJSON (Object o)
      _ -> fail "Unknown Type"

jsonOpts :: Options
jsonOpts = defaultOptions { fieldLabelModifier = capitalize }
  where
    capitalize []     = []
    capitalize (x:xs) = toUpper x : xs

injectType :: Text -> Value -> Value
injectType type_ (Object o) =
  Object $ HM.insert "Type" (String type_) o
injectType _ other = other

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
