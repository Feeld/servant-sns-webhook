{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
module Network.AWS.SNS.Webhook.Types (
  Message(..)
, Confirmation(..)
, Notification(..)

, Url (..)
, SignedText (..)
, Signature (..)
, Embedded (..)
, messageSigningCertURL
, messageSignature
, messageSignedText
, embedded
) where

import           Control.Lens            (Iso', Lens', iso, lens)
import           Data.Aeson              (FromJSON (..), Object,
                                          Options (fieldLabelModifier),
                                          ToJSON (..), Value (Object, String),
                                          defaultOptions, eitherDecodeStrict,
                                          encode, genericParseJSON,
                                          genericToJSON, withObject, withText,
                                          (.:))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Base64  as B64
import           Data.ByteString.Builder (byteString, shortByteString,
                                          toLazyByteString)
import           Data.Char               (toUpper)
import           Data.Hashable           (Hashable (hashWithSalt))
import qualified Data.HashMap.Strict     as HM
import           Data.List               (foldl')
import           Data.String.Conv        (toS)
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           GHC.Generics            (Generic)
import           Network.URI             (URI, parseURIReference, uriToString)

newtype SignedText = SignedText ByteString
  deriving (Eq, Show)

newtype Signature = Signature ByteString
  deriving (Eq, Show)

instance ToJSON Signature where
  toJSON (Signature s) = String $ toS $ B64.encode s

instance FromJSON Signature where
  parseJSON = withText "Signature" $ \s ->
    either fail (pure . Signature) $ B64.decode (toS s)


newtype Url = Url URI
  deriving stock Generic
  deriving newtype (Eq, Show, Ord)

instance Hashable Url where
  hashWithSalt n (Url uri) = hashWithSalt n (uriToString id uri "")

instance ToJSON Url where
  toJSON (Url u) = toJSON $ uriToString id u ""

instance FromJSON Url where
  parseJSON = withText "Url" $
    maybe (fail "invalid URI") (pure . Url) . parseURIReference . toS

data Message a
  = MsgNotification             SignedText (Notification a)
  | MsgSubscriptionConfirmation SignedText Confirmation
  | MsgUnsubscribeConfirmation  SignedText Confirmation
  deriving (Eq, Show)

messageSigningCertURL :: Lens' (Message a) Url
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
  , subscribeURL     :: Url
  , message          :: Text
  , timestamp        :: UTCTime
  , signatureVersion :: Text
  , signature        :: Signature
  , signingCertURL   :: Url
  } deriving (Eq, Show, Generic)

instance FromJSON Confirmation where
  parseJSON = genericParseJSON jsonOpts

data Notification a = Notification
  { messageId        :: Text
  , topicArn         :: Text
  , subject          :: Maybe Text
  , unsubscribeURL   :: Url
  , message          :: a
  , timestamp        :: UTCTime
  , signatureVersion :: Text
  , signature        :: Signature
  , signingCertURL   :: Url
  } deriving (Eq, Show, Generic)


instance FromJSON a => FromJSON (Notification a) where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON a => ToJSON (Notification a) where
  toJSON = injectType "Notification"
         . genericToJSON jsonOpts

instance FromJSON a => FromJSON (Message a) where
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

newtype Embedded a = Embedded { unEmbedded :: a }
  deriving stock (Show, Generic)
  deriving newtype Eq

instance FromJSON a => FromJSON (Embedded a) where
  parseJSON =
    withText "Embedded"
      (fmap Embedded . either fail pure . eitherDecodeStrict . toS)

instance ToJSON a => ToJSON (Embedded a) where
  toJSON = String . toS . Data.Aeson.encode . unEmbedded

embedded :: Iso' (Embedded a) a
embedded = iso unEmbedded Embedded
{-# INLINE embedded #-}

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
notificationSignedText = getSignedText
  [ "Message"
  , "MessageId"
  , "Subject"
  , "SubscribeURL"
  , "Timestamp"
  , "TopicArn"
  , "Type"
  ]

confirmationSignedText :: Object -> SignedText
confirmationSignedText = getSignedText
  [ "Message"
  , "MessageId"
  , "Subject"
  , "SubscribeURL"
  , "Timestamp"
  , "Token"
  , "TopicArn"
  , "Type"
  ]

getSignedText :: [ByteString] -> Object -> SignedText
getSignedText keys o = SignedText . toS . toLazyByteString $ foldl' go mempty keys
  where
  go !acc k = (acc <>) $ maybe mempty (kvLine k) (toS k `HM.lookup` o)
  kvLine k (String v) =
       byteString k
    <> shortByteString "\n"
    <> byteString (toS v)
    <> shortByteString "\n"
  kvLine _ _ = mempty
