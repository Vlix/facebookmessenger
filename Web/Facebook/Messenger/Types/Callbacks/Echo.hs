module Web.Facebook.Messenger.Types.Callbacks.Echo where


import           Control.Applicative  ((<|>))
import           Data.Text
import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)
import qualified Data.HashMap.Strict  as HM

import           Web.Facebook.Messenger.Types.Requests.Attachment (RequestAttachment)
import           Web.Facebook.Messenger.Types.Callbacks.Message   (CallbackQuickReply(..))
import           Web.Facebook.Messenger.Types.Static


-- --------------- --
--  ECHO CALLBACK  --
-- --------------- --

data Echo =
  EchoText
    { echo_appid      :: Maybe Int -- ID of the app from which the message was sent
  -- app_id might be Number, documentation is ambiguous <--- IS ACTUALLY A NUMBER, GODDAMNIT
    , echo_metadata   :: Maybe Text -- Custom string passed to the Send API as the metadata field
    , echo_mid        :: Text -- Message ID
    , echo_text       :: Text -- Text of message
    , echo_quickreply :: Maybe CallbackQuickReply
    , echo_seq        :: Maybe Int  -- Sequence number
    }
  | EchoAttachment
    { echo_appid       :: Maybe Int
    , echo_metadata    :: Maybe Text
    , echo_mid         :: Text
    , echo_attachments :: [RequestAttachment] -- Template payload as described in the Send API Reference (.Callbacks.Requests)
    , echo_quickreply  :: Maybe CallbackQuickReply
    , echo_seq         :: Maybe Int
    }
  | EchoFallback
    { echo_appid      :: Maybe Int
    , echo_metadata   :: Maybe Text
    , echo_mid        :: Text
    , echo_fallback   :: [Fallback]
    , echo_quickreply :: Maybe CallbackQuickReply
    , echo_seq        :: Maybe Int
   } deriving (Eq, Show)

data Fallback = Fallback
  { fallback_title   :: Maybe Text -- Title of attachment (optional)
  , fallback_url     :: Maybe Text -- URL of attachment (optional)
  , fallback_payload :: Maybe Text -- Payload of attachment (optional)
  } deriving (Eq, Show)


-- ---------------- --
--  ECHO INSTANCES  --
-- ---------------- --

instance FromJSON Echo where
  parseJSON (Object o) = case HM.lookup "is_echo" o of
    Just (Bool True) ->
      EchoText <$> o .:? "app_id"
               <*> o .:? "metadata"
               <*> o .: "mid"
               <*> o .: "text"
               <*> o .:? "quick-reply"
               <*> o .:? "seq"
      <|> EchoAttachment <$> o .:? "app_id"
                         <*> o .:? "metadata"
                         <*> o .: "mid"
                         <*> o .: "attachments"
                         <*> o .:? "quick-reply"
                         <*> o .:? "seq"
      <|> EchoFallback <$> o .:? "app_id"
                       <*> o .:? "metadata"
                       <*> o .: "mid"
                       <*> o .: "attachments"
                       <*> o .:? "quick-reply"
                       <*> o .:? "seq"
    _ -> fail "expected is_echo to be true in Echo object"
  parseJSON wat = typeMismatch "Echo" wat

instance FromJSON Fallback where
  parseJSON (Object o) = Fallback <$> o .:? "title"
                                  <*> o .:? "url"
                                  <*> o .:? "payload"
  parseJSON wat = typeMismatch "Fallback" wat 


instance ToJSON Echo where
  toJSON (EchoText appid metadata mid txt quickreply seq') =
    object' [ "is_echo"     .=! Bool True
            , "mid"         .=! mid
            , "text"        .=! txt
            , "app_id"      .=! appid
            , "metadata"    .=!! metadata
            , "quick-reply" .=!! quickreply
            , "seq"         .=!! seq'
            ]
  toJSON (EchoAttachment appid metadata mid attachments quickreply seq') =
    object' [ "is_echo"     .=! Bool True
            , "attachments" .=! attachments
            , "mid"         .=! mid
            , "app_id"      .=! appid
            , "metadata"    .=!! metadata
            , "quick-reply" .=!! quickreply
            , "seq"         .=!! seq'
            ]
  toJSON (EchoFallback appid metadata mid fallback quickreply seq') =
    object' [ "is_echo"     .=! Bool True
            , "mid"         .=! mid
            , "attachments" .=! fallback
            , "app_id"      .=! appid
            , "metadata"    .=!! metadata
            , "quick-reply" .=!! quickreply
            , "seq"         .=!! seq'
            ]

instance ToJSON Fallback where
  toJSON (Fallback title url payload) =
    object' [ "type"    .=! String "fallback"
            , "title"   .=!! title
            , "url"     .=!! url
            , "payload" .=!! payload
            ]
