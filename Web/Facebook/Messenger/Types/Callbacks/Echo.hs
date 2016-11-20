module Web.Facebook.Messenger.Types.Callbacks.Echo where


import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)

import Web.Facebook.Messenger.Types.Requests.Attachment (RequestAttachment)
import Web.Facebook.Messenger.Types.Callbacks.Message   (CallbackQuickReply(..))
-- --------------- --
--  ECHO CALLBACK  --
-- --------------- --

data Echo =
    EchoText
    { echo_isecho     :: Bool -- Indicates the message sent from the page itself
    , echo_appid      :: Maybe Int -- ID of the app from which the message was sent
  -- app_id might be Number, documentation is ambiguous <--- IS ACTUALLY A NUMBER
    , echo_metadata   :: Maybe Text -- Custom string passed to the Send API as the metadata field
    , echo_quickreply :: Maybe CallbackQuickReply
    , echo_mid        :: Text -- Message ID
    , echo_seq        :: Int  -- Sequence number
    , echo_text       :: Text -- Text of message
    }
  | EchoAttachment
    { echo_isecho      :: Bool
    , echo_appid       :: Maybe Int
    , echo_metadata    :: Maybe Text
    , echo_quickreply  :: Maybe CallbackQuickReply
    , echo_mid         :: Text
    , echo_seq         :: Int
    , echo_attachments :: [RequestAttachment] -- Template payload as described in the Send API Reference (.Callbacks.Requests)
    }
  | EchoFallback
    { echo_isecho     :: Bool
    , echo_appid      :: Maybe Int
    , echo_metadata   :: Maybe Text
    , echo_quickreply :: Maybe CallbackQuickReply
    , echo_mid        :: Text
    , echo_seq        :: Int
    , echo_fallback   :: [Fallback]
  }
  deriving (Eq, Show)

data Fallback = Fallback
    { fallback_title   :: Maybe Text -- Title of attachment (optional)
    , fallback_url     :: Maybe Text -- URL of attachment (optional)
    , fallback_payload :: Maybe Text -- Payload of attachment (optional)
    }
  deriving (Eq, Show)


-- ---------------- --
--  ECHO INSTANCES  --
-- ---------------- --

instance FromJSON Echo where
    parseJSON (Object o) = EchoText <$> o .: "is_echo"
                                    <*> o .:? "app_id"
                                    <*> o .:? "metadata"
                                    <*> o .:? "quick-reply"
                                    <*> o .: "mid"
                                    <*> o .: "seq"
                                    <*> o .: "text"
                       <|> EchoAttachment <$> o .: "is_echo"
                                          <*> o .:? "app_id"
                                          <*> o .:? "metadata"
                                          <*> o .:? "quick-reply"
                                          <*> o .: "mid"
                                          <*> o .: "seq"
                                          <*> o .: "attachments"
                       <|> EchoFallback <$> o .: "is_echo"
                                        <*> o .:? "app_id"
                                        <*> o .:? "metadata"
                                        <*> o .:? "quick-reply"
                                        <*> o .: "mid"
                                        <*> o .: "seq"
                                        <*> o .: "fallback"
    parseJSON wat = typeMismatch "Echo" wat

instance FromJSON Fallback where
    parseJSON (Object o) = Fallback <$> o .:? "title"
                                    <*> o .:? "url"
                                    <*> o .:? "payload"
    parseJSON wat = typeMismatch "Fallback" wat 


instance ToJSON Echo where
    toJSON (EchoText isecho appid metadata
                     mid    seq'  txt     quickreply) = object [ "is_echo"     .= isecho
                                                               , "app_id"      .= appid
                                                               , "metadata"    .= metadata
                                                               , "quick-reply" .= quickreply
                                                               , "mid"         .= mid
                                                               , "seq"         .= seq'
                                                               , "text"        .= txt
                                                               ]
    toJSON (EchoAttachment isecho appid metadata
                           mid    seq'  attachments quickreply) = object [ "is_echo"     .= isecho
                                                                         , "app_id"      .= appid
                                                                         , "metadata"    .= metadata
                                                                         , "quick-reply" .= quickreply
                                                                         , "mid"         .= mid
                                                                         , "seq"         .= seq'
                                                                         , "attachments" .= attachments
                                                                         ]
    toJSON (EchoFallback isecho appid metadata
                         mid    seq'  fallback quickreply) = object [ "is_echo"     .= isecho
                                                                    , "app_id"      .= appid
                                                                    , "metadata"    .= metadata
                                                                    , "quick-reply" .= quickreply
                                                                    , "mid"         .= mid
                                                                    , "seq"         .= seq'
                                                                    , "attachments" .= fallback
                                                                    ]

instance ToJSON Fallback where
    toJSON (Fallback title url payload) = object [ "type"    .= String "fallback"
                                                 , "title"   .= title
                                                 , "url"     .= url
                                                 , "payload" .= payload
                                                 ]
