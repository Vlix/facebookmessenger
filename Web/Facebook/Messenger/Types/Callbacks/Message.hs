module Web.Facebook.Messenger.Types.Callbacks.Message where


import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as HM
import Data.Text

import Web.Facebook.Messenger.Types.Requests.Templates (GenericTemplateElement)
import Web.Facebook.Messenger.Types.Static


-- ------------------ --
--  MESSAGE CALLBACK  --
-- ------------------ --

type MessageId = Text

data Message = Message MessageId MessageContent
  deriving (Eq, Show)

data MessageContent =
    MessageText { mtText :: Text -- Text of message
                , mtQuickreply :: Maybe CallbackQuickReply } -- Optional custom data provided by the sending app
  | MessageAttachment { maAttachments :: [CallbackAttachment] } -- Array containing attachment data
  | MessageSticker { msAttachments :: [StickerAttachment] -- Array containing attachment data
                   , msStickerId  :: Integer } -- Sticker ID
  | MessageLocation { mlCoords :: [CallbackLocation] } -- Array containing Location Quick Reply Callback (probably just 1)
  | MessageFallback { mfText :: Text
                    , mfAttachments :: [CallbackFallback] } -- URL scraped messages
    deriving (Eq, Show)

newtype CallbackQuickReply =
          CallbackQuickReply { cbQR :: Text }
  deriving (Eq, Show)

newtype StickerAttachment =
          StickerAttachment { sticker :: CallbackStickerPayload }
  deriving (Eq, Show)

data CallbackStickerPayload = CallbackStickerPayload
  { cspStickerUrl :: Text -- URL of the file
  , cspStickerId :: Integer } -- sticker_id
  deriving (Eq, Show)

data CallbackAttachment =
    CallbackAttachment
      { caType :: AttachmentType
      , caPayload :: CallbackMultimediaPayload }
  | CallbackAttachmentTemplate
      { caTemplateTitle :: Maybe Text
      , caTemplateSubtitle :: Maybe Text
      , caTemplateUrl :: Maybe Text
      , caTemplatePayload :: CallbackTemplate }
  deriving (Eq, Show)

newtype CallbackMultimediaPayload =
          CallbackMultimediaPayload { cmpUrl :: Text } -- URL of the file
    deriving (Eq, Show)

data CallbackTemplate = CallbackTemplate
  { ctSharable :: Maybe Bool
  , ctElements :: [GenericTemplateElement] }
  deriving (Eq, Show)

newtype CallbackLocation =
          CallbackLocation { clPayload :: CallbackLocationPayload }
  deriving (Eq, Show)

newtype CallbackLocationPayload =
          CallbackLocationPayload { clpCoords :: CallbackCoordinates }
    deriving (Eq, Show)

data CallbackCoordinates = CallbackCoordinates
  { ccLat :: Double -- Latitude
  , ccLong :: Double } -- Longitude
  deriving (Eq, Show)

data CallbackFallback = CallbackFallback
    { cfTitle :: Text
    , cfURL :: Text }
  deriving (Eq, Show)


-- ------------------- --
--  MESSAGE INSTANCES  --
-- ------------------- --

instance FromJSON Message where
  parseJSON v@(Object o) =
      Message <$> o .: "mid"
                      <*> parseJSON v
  parseJSON wat = typeMismatch "Message" wat


instance FromJSON MessageContent where
  parseJSON = withObject "MessageContent" $ \o ->
        MessageFallback <$> o .: "text"
                        <*> o .: "attachments"
    <|> MessageText <$> o .: "text"
                    <*> o .:? "quick_reply"
    <|> MessageSticker <$> o .: "attachments"
                       <*> o .: "sticker_id"
    <|> MessageLocation <$> o .: "attachments"
    <|> MessageAttachment <$> o .: "attachments"

instance FromJSON CallbackQuickReply where
  parseJSON = withObject "CallbackQuickReply" $ \o -> 
      CallbackQuickReply <$> o .: "payload"

instance FromJSON StickerAttachment where
  parseJSON = withObject "StickerAttachment" $ \o -> do
      typ <- o .: "type"
      case typ of
        String "image" -> StickerAttachment <$> o .: "payload"
        _ -> fail "StickerAttachment: no \"image\" type"

instance FromJSON CallbackStickerPayload where
  parseJSON = withObject "CallbackStickerPayload" $ \o ->
      CallbackStickerPayload <$> o .: "url"
                             <*> o .: "sticker_id"

instance FromJSON CallbackAttachment where
  parseJSON = withObject "CallbackAttachment" $ \o ->
      case HM.lookup "type" o of
        Just (String "template") ->
            CallbackAttachmentTemplate <$> o .: "title"
                                       <*> o .:? "subtitle"
                                       <*> o .: "url"
                                       <*> o .: "payload"
        _ -> CallbackAttachment <$> o .: "type"
                                <*> o .: "payload"

instance FromJSON CallbackMultimediaPayload where
  parseJSON = withObject "CallbackMultimediaPayload" $ \o ->
      CallbackMultimediaPayload <$> o .: "url"

instance FromJSON CallbackTemplate where
  parseJSON = withObject "CallbackTemplate" $ \o ->
      case HM.lookup "template_type" o of
        Just (String "generic") ->
            CallbackTemplate <$> o .: "sharable"
                             <*> o .:? "elements" .!= []
        _ -> fail "Only known template is 'generic' in CallbackTemplate"

instance FromJSON CallbackLocation where
  parseJSON = withObject "CallbackLocation" $ \o -> do
      typ <- o .: "type"
      case typ of
        String "location" ->
            CallbackLocation <$> o .: "payload"
        _ -> fail "CallbackLocation: no \"location\" type"

instance FromJSON CallbackLocationPayload where
  parseJSON = withObject "CallbackLocationPayload" $ \o ->
      CallbackLocationPayload <$> o .: "coordinates" 

instance FromJSON CallbackCoordinates where
  parseJSON = withObject "CallbackCoordinates" $ \o ->
      CallbackCoordinates <$> o .: "lat"
                          <*> o .: "long"

instance FromJSON CallbackFallback where
  parseJSON = withObject "CallbackFallback" $ \o -> do
      typ <- o .: "type"
      case typ of
        String "fallback" ->
            CallbackFallback <$> o .: "title"
                             <*> o .: "URL"
        _ -> fail "CallbackFallback: no \"fallback\" type"


instance ToJSON Message where
  toJSON (Message ident content) =
      case toJSON content of
        Object o -> Object $ HM.insert "mid" (String ident) o
        x -> x -- This should never happen. Content should be an object

instance ToJSON MessageContent where
  toJSON (MessageText text qreply) =
      object' [ "text"        .=! text
              , "quick_reply" .=!! qreply
              ]
  toJSON (MessageSticker attachments stickerId) =
      object [ "attachments" .= attachments
             , "sticker_id"  .= stickerId
             ]
  toJSON (MessageAttachment attachments) =
      object [ "attachments" .= attachments
             ]
  toJSON (MessageLocation coords) =
      object [ "attachments" .= coords
             ]
  toJSON (MessageFallback txt fallbacks) =
      object [ "text" .= txt
             , "attachments" .= fallbacks
             ]
  
instance ToJSON CallbackQuickReply where
  toJSON (CallbackQuickReply payload) =
      object [ "payload" .= payload ]

instance ToJSON StickerAttachment where
  toJSON (StickerAttachment payload) =
      object [ "type" .= String "image"
             , "payload" .= payload
             ]      

instance ToJSON CallbackStickerPayload where
  toJSON (CallbackStickerPayload url stickerId) =
      object [ "url"        .= url
             , "sticker_id" .= stickerId
             ]

instance ToJSON CallbackAttachment where
  toJSON (CallbackAttachment typ payload) =
      object [ "type"    .= typ
             , "payload" .= payload
             ]
  toJSON (CallbackAttachmentTemplate title subtitle url payload) =
      object [ "type"     .= String "template"
             , "title"    .= title
             , "subtitle" .= subtitle
             , "url"      .= url
             , "payload"  .= payload
             ]

instance ToJSON CallbackMultimediaPayload where
  toJSON (CallbackMultimediaPayload url) =
      object [ "url" .= url ]

instance ToJSON CallbackTemplate where
  toJSON (CallbackTemplate sharable elements) =
      object [ "template_type" .= String "generic"
             , "sharable"      .= sharable
             , "elements"      .= elements
             ]

instance ToJSON CallbackLocation where
  toJSON (CallbackLocation payload) =
      object [ "type"    .= String "location"
             , "payload" .= payload
             ]

instance ToJSON CallbackFallback where
  toJSON (CallbackFallback title url) =
      object [ "type" .= String "fallback"
             , "payload" .= Null
             , "title" .= title
             , "URL" .= url
             ]

instance ToJSON CallbackLocationPayload where
  toJSON (CallbackLocationPayload coords) =
      object [ "coordinates" .= coords ]

instance ToJSON CallbackCoordinates where
  toJSON (CallbackCoordinates lat long) =
      object [ "lat"  .= lat
             , "long" .= long
             ]
