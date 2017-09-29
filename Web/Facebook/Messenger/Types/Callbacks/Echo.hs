module Web.Facebook.Messenger.Types.Callbacks.Echo (
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
import Data.Text

import Web.Facebook.Messenger.Types.Requests.Attachment (RequestAttachment)
import Web.Facebook.Messenger.Types.Callbacks.Message (CallbackQuickReply(..))
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
    , eQuickReply :: Maybe CallbackQuickReply
    , eSeq :: Maybe Integer -- Sequence number
    , eContent :: EchoContent
    } deriving (Eq, Show)

data EchoContent = EText EchoText
                 | EAttachment EchoAttachment
                 | EFallback EchoFallback
  deriving (Eq, Show)

newtype EchoText = EchoText { eText :: Text } -- Text of message
  deriving (Eq, Show)

newtype EchoAttachment = EchoAttachment { eAttachments :: [RequestAttachment] }
    -- Template payload as described in the Send API Reference (.Callbacks.Requests)
  deriving (Eq, Show)

newtype EchoFallback = EchoFallback { eFallback :: [Fallback] }
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
  parseJSON = withObject "EchoAttachment" $ \o ->
      EchoAttachment <$> o .: "attachments"

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
        txt@(EText x) -> "text" .=! eText x
        att@(EAttachment x) -> "attachments" .=! eAttachments x
        flb@(EFallback x) -> "attachments" .=! eFallback x

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
