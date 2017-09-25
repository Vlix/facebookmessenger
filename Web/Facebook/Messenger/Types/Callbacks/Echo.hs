module Web.Facebook.Messenger.Types.Callbacks.Echo where


import Control.Applicative ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as HM

import Web.Facebook.Messenger.Types.Requests.Attachment (RequestAttachment)
import Web.Facebook.Messenger.Types.Callbacks.Message   (CallbackQuickReply(..))
import Web.Facebook.Messenger.Types.Static


-- --------------- --
--  ECHO CALLBACK  --
-- --------------- --

data Echo = Echo
    { eIsEcho :: Bool
    , eAppId :: Maybe Integer -- ID of the app from which the message was sent
  -- app_id might be Number, documentation is ambiguous <--- IS ACTUALLY A NUMBER, GODDAMNIT
    , eMetaData :: Maybe Text -- Custom string passed to the Send API as the metadata field
    , eMid :: Text -- Message ID
    , eContent :: EchoContent
    , eQuickReply :: Maybe CallbackQuickReply
    , eSeq :: Maybe Integer } -- Sequence number
  deriving (Eq, Show)

data EchoContent =
    EchoText { eText :: Text } -- Text of message
  | EchoAttachment { eAttachments :: [RequestAttachment] } -- Template payload as described in the Send API Reference (.Callbacks.Requests)
  | EchoFallback { eFallback :: [Fallback] }
  deriving (Eq, Show)

data Fallback = Fallback
  { fTitle :: Maybe Text -- Title of attachment (optional)
  , fUrl :: Maybe Text -- URL of attachment (optional)
  , fPayload :: Maybe Text -- Payload of attachment (optional)
  } deriving (Eq, Show)


-- ---------------- --
--  ECHO INSTANCES  --
-- ---------------- --

instance FromJSON Echo where
  parseJSON v@(Object o) =
    case "is_echo" `HM.lookup` o of
      Just (Bool b) ->
        Echo <$> pure b
             <*> o .:? "app_id"
             <*> o .:? "metadata"
             <*> o .: "mid"
             <*> parseJSON v
             <*> o .:? "quick-reply"
             <*> o .:? "seq"
      _ -> fail "Echo: invalid \"is_echo\" field"
  parseJSON wat = typeMismatch "Echo" wat

instance FromJSON EchoContent where
  parseJSON = withObject "EchoContent" $ \o ->
          EchoText <$> o .: "text"
      <|> EchoAttachment <$> o .: "attachments"
      <|> EchoFallback <$> o .: "attachments"

instance FromJSON Fallback where
  parseJSON = withObject "Fallback" $ \o ->
      case "type" `HM.lookup` o of
        Just (String "fallback") ->
            Fallback <$> o .:? "title"
                     <*> o .:? "url"
                     <*> o .:? "payload"
        _ -> fail "Fallback: no \"fallback\" type"


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
      txt@EchoText{} -> "text" .=! eText txt
      att@EchoAttachment{} -> "attachments" .=! eAttachments att
      flb@EchoFallback{} -> "attachments" .=! eFallback flb

instance ToJSON EchoContent where
  toJSON (EchoText txt) = object' [ "text" .=! txt]
  toJSON (EchoAttachment atts) = object [ "attachments" .= atts ]
  toJSON (EchoFallback fbs) = object [ "attachments" .= fbs ]

instance ToJSON Fallback where
  toJSON (Fallback title url payload) =
    object' [ "type"    .=! String "fallback"
            , "title"   .=!! title
            , "url"     .=!! url
            , "payload" .=!! payload
            ]
