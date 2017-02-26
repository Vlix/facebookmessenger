module Web.Facebook.Messenger.Types.Callbacks.Echo where


import           Control.Applicative  ((<|>))
import           Data.Text
import           Data.Aeson
import qualified Data.HashMap.Strict  as HM

import           Web.Facebook.Messenger.Types.Requests.Attachment (RequestAttachment)
import           Web.Facebook.Messenger.Types.Callbacks.Message   (CallbackQuickReply(..))
import           Web.Facebook.Messenger.Types.Static


-- --------------- --
--  ECHO CALLBACK  --
-- --------------- --

data Echo =
  EchoText
    { echo_appid      :: Maybe Integer  -- ID of the app from which the message was sent
  -- app_id might be Number, documentation is ambiguous <--- IS ACTUALLY A NUMBER, GODDAMNIT
    , echo_metadata   :: Maybe Text     -- Custom string passed to the Send API as the metadata field
    , echo_mid        :: Text           -- Message ID
    , echo_text       :: Text           -- Text of message
    , echo_quickreply :: Maybe CallbackQuickReply
    , echo_seq        :: Maybe Integer  -- Sequence number
    }
  | EchoAttachment
    { echo_appid       :: Maybe Integer
    , echo_metadata    :: Maybe Text
    , echo_mid         :: Text
    , echo_attachments :: [RequestAttachment] -- Template payload as described in the Send API Reference (.Callbacks.Requests)
    , echo_quickreply  :: Maybe CallbackQuickReply
    , echo_seq         :: Maybe Integer
    }
  | EchoFallback
    { echo_appid      :: Maybe Integer
    , echo_metadata   :: Maybe Text
    , echo_mid        :: Text
    , echo_fallback   :: [Fallback]
    , echo_quickreply :: Maybe CallbackQuickReply
    , echo_seq        :: Maybe Integer
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
  parseJSON = withObject "Echo" $ \o ->
    if "is_echo" `HM.lookup` o == Just (Bool True)
      then
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
      else fail "expected is_echo to be true in Echo object"

instance FromJSON Fallback where
  parseJSON = withObject "Fallback" $ \o ->
    Fallback <$> o .:? "title"
             <*> o .:? "url"
             <*> o .:? "payload"


instance ToJSON Echo where
  toJSON echo = object' $ extra : basis
   where
    basis = [ "is_echo"     .=! Bool True
            , "mid"         .=! echo_mid echo
            , "app_id"      .=! echo_appid echo
            , "metadata"    .=!! echo_metadata echo
            , "quick-reply" .=!! echo_quickreply echo
            , "seq"         .=!! echo_seq echo
            ]
    extra = case echo of
      txt@EchoText{}       -> "text" .=! echo_text txt
      att@EchoAttachment{} -> "attachments" .=! echo_attachments att
      flb@EchoFallback{}   -> "attachments" .=! echo_fallback flb

instance ToJSON Fallback where
  toJSON (Fallback title url payload) =
    object' [ "type"    .=! String "fallback"
            , "title"   .=!! title
            , "url"     .=!! url
            , "payload" .=!! payload
            ]
