{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.Echo
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will occur when a message has been sent by your page.
You may receive text messsages or messages with attachments (image, video, audio, template or fallback).
The payload will also include an optional custom metadata sent by the sender, and the corresponding app_id.
You can subscribe to this callback by selecting the @"message_echoes"@ field when setting up your webhook.

Multiple types of messages are supported:

* Text message
* Message with image, audio, video or file attachment
* Message with template attachment
* Message with fallback attachment

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/message-echoes
-}
module Web.Facebook.Messenger.Types.Callbacks.Echo (
  -- * Echo Callback
  Echo (..)
  , EchoContent (..)
  , EchoText (..)
  , EchoAttachment (..)
  , EchoFallback (..)
  , Fallback (..)
  )
where


import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Requests
import Web.Facebook.Messenger.Types.Requests.Attachment (RequestAttachment)
import Web.Facebook.Messenger.Types.Callbacks.Message (CallbackQuickReply(..), MessageId)


-- --------------- --
--  ECHO CALLBACK  --
-- --------------- --

-- | Echo callback sent when a message is sent as the page. This can also happen if an admin/editor of the page responds manually to a user.
data Echo = Echo
    { eIsEcho :: Bool -- ^ Indicates the message sent from the page itself
    , eAppId :: Maybe Integer
    -- ^ ID of the app from which the message was sent.
    -- (`eAppId` was a @Number@ at the time of making this module /2017-10-03/)
    , eMetaData :: Maybe Text
    -- ^ Custom string passed to the Send API as the metadata field.
    -- Only present if the metadata property was set in the original message.
    , eMid :: MessageId -- ^ Message ID
    , eQuickReply :: Maybe CallbackQuickReply -- ^ Present if the message sent had Quick Replies in it
    , eSeq :: Maybe Integer -- ^ Sequence number
    , eContent :: EchoContent -- ^ Contents of the Echo callback
    } deriving (Eq, Show)

-- | Different kinds of Echo callbacks
data EchoContent = EText EchoText -- ^ Regular message
                 | EAttachment EchoAttachment -- ^ Attachment message
                 | EFallback EchoFallback -- ^ Any other message
  deriving (Eq, Show)

-- | Text message
newtype EchoText = EchoText { eText :: Text }
  deriving (Eq, Show)

-- | Attachment message. `RequestAttachment` as described in the Send API Reference ("Requests")
newtype EchoAttachment = EchoAttachment { eAttachments :: [RequestAttachment] }
  deriving (Eq, Show)

-- | Fallback message
newtype EchoFallback = EchoFallback { eFallback :: [Fallback] }
  deriving (Eq, Show)

-- | Fallback template-like contents. Just a mess of stuff that might mean something.
data Fallback = Fallback
    { fTitle :: Maybe Text -- ^ Title of attachment (optional)
    , fUrl :: Maybe URL -- ^ URL of attachment (optional)
    , fPayload :: Maybe Text -- ^ Payload of attachment (optional)
    } deriving (Eq, Show)


-- ---------------- --
--  ECHO INSTANCES  --
-- ---------------- --

instance FromJSON Echo where
  parseJSON = withObject "Echo" $ \o ->
      Echo <$> o .: "is_echo"
           <*> o .:? "app_id"
           <*> o .:? "metadata"
           <*> o .: "mid"
           <*> o .:? "quick-reply"
           <*> o .:? "seq"
           <*> parseJSON (Object o)

instance FromJSON EchoContent where
  parseJSON = withObject "EchoContent" $ \o ->
        EText <$> parseJSON (Object o)
    <|> EAttachment <$> parseJSON (Object o)
    <|> EFallback <$> parseJSON (Object o)

instance FromJSON EchoText where
  parseJSON = withObject "EchoText" $ \o ->
      EchoText <$> o .: "text"

instance FromJSON EchoAttachment where
  parseJSON = withObject "EchoAttachment" $ \o -> do
      atts <- o .: attField
      let actualAtts = filter (/= Object mempty) (atts :: [Value])
          newObj = HM.insert attField (toJSON actualAtts) o
      EchoAttachment <$> newObj .: attField
    where attField = "attachments"

instance FromJSON EchoFallback where
  parseJSON = withObject "EchoFallback" $ \o ->
      EchoFallback <$> o .: "attachments"


instance FromJSON Fallback where
  parseJSON = checkValue
      "Fallback"
      "type"
     ("fallback" :: Text)
     $ \o -> Fallback <$> o .:? "title"
                      <*> o .:? "url"
                      <*> o .:? "payload"


instance ToJSON Echo where
  toJSON echo = object' $ extra : basis
   where
    basis = [ "is_echo"     .=! eIsEcho echo
            , "mid"         .=! eMid echo
            , "app_id"      .=! eAppId echo
            , "metadata"    .=!! eMetaData echo
            , "quick-reply" .=!! eQuickReply echo
            , "seq"         .=!! eSeq echo
            ]
    extra = case eContent echo of
        EText x -> "text" .=! eText x
        EAttachment x -> "attachments" .=! eAttachments x
        EFallback x -> "attachments" .=! eFallback x

instance ToJSON EchoContent where
  toJSON (EText x) = toJSON x
  toJSON (EAttachment x) = toJSON x
  toJSON (EFallback x) = toJSON x

instance ToJSON EchoText where
  toJSON (EchoText txt) =
      object ["text" .= txt]

instance ToJSON EchoAttachment where
  toJSON (EchoAttachment atts) =
      object ["attachments" .= atts]

instance ToJSON EchoFallback where
  toJSON (EchoFallback fbs) =
      object ["attachments" .= fbs]


instance ToJSON Fallback where
  toJSON (Fallback title url payload) =
      object' [ "type"    .=! String "fallback"
              , "title"   .=!! title
              , "url"     .=!! url
              , "payload" .=!! payload
              ]
