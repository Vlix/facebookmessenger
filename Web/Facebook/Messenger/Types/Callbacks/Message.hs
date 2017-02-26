module Web.Facebook.Messenger.Types.Callbacks.Message where


import           Control.Applicative  ((<|>))
import           Data.Aeson
import qualified Data.HashMap.Strict  as HM
import           Data.Text

import           Web.Facebook.Messenger.Types.Requests.Templates
import           Web.Facebook.Messenger.Types.Static


-- ------------------ --
--  MESSAGE CALLBACK  --
-- ------------------ --

data CallbackMessage =
    CallbackMessageText
    { cb_msg_mid        :: Text                     -- Message ID
    , cb_msg_text       :: Text                     -- Text of message
    , cb_msg_quickreply :: Maybe CallbackQuickReply -- Optional custom data provided by the sending app
    , cb_msg_seq        :: Maybe Integer            -- Message sequence number
    }
  | CallbackMessageAttachment
    { cb_msg_mid         :: Text                 -- Message ID
    , cb_msg_attachments :: [CallbackAttachment] -- Array containing attachment data
    , cb_msg_seq         :: Maybe Integer        -- Message sequence number
    }
  | CallbackMessageSticker
    { cb_msg_mid         :: Text                 -- Message ID
    , cb_msg_attachments :: [CallbackAttachment] -- Array containing attachment data
    , cb_msg_sticker_id  :: Integer              -- Sticker ID
    , cb_msg_seq         :: Maybe Integer        -- Message sequence number
    }
  | CallbackMessageLocation
    { cb_msg_mid    :: Text               -- Message ID
    , cb_msg_coords :: [CallbackLocation] -- Array containing Location Quick Reply Callback (probably just 1)
    , cb_msg_seq    :: Maybe Integer      -- Message sequence number
    } deriving (Eq, Show)

newtype CallbackQuickReply = CallbackQuickReply { cb_quick_reply_payload :: Text }
  deriving (Eq, Show)

data CallbackAttachment
  = CallbackSticker
  { cb_attachment_sticker :: CallbackStickerPayload }
  | CallbackAttachment
  { cb_attachment_type    :: AttachmentType
  , cb_attachment_payload :: CallbackMultimediaPayload
  }
  | CallbackAttachmentTemplate
  { cb_attachment_template_title    :: Maybe Text
  , cb_attachment_template_subtitle :: Maybe Text
  , cb_attachment_template_url      :: Maybe Text
  , cb_attachment_template_payload  :: CallbackTemplate
  } deriving (Eq, Show)

data CallbackTemplate = CallbackGenericTemplate
  { cb_template_sharable :: Maybe Bool
  , cb_template_elements :: [GenericTemplateElement]
  } deriving (Eq, Show)

data CallbackLocation = CallbackLocation
  { cb_attachment_title  :: Text
  , cb_attachment_url    :: Text
  , cb_attachment_coords :: CallbackLocationPayload
  } deriving (Eq, Show)

newtype CallbackMultimediaPayload = CallbackMultimediaPayload
  { cb_multimedia_payload_url :: Text } -- URL of the file
    deriving (Eq, Show)

data CallbackStickerPayload = CallbackStickerPayload
  { cb_multimedia_sticker_url :: Text   -- URL of the file
  , cb_multimedia_sticker_id  :: Integer -- sticker_id
  } deriving (Eq, Show)

newtype CallbackLocationPayload = CallbackLocationPayload
  { cb_coordinates_payload :: CallbackCoordinates }
    deriving (Eq, Show)

data CallbackCoordinates = CallbackCoordinates
  { cb_coords_lat  :: Double -- Latitude
  , cb_coords_long :: Double -- Longitude
  } deriving (Eq, Show)


-- ------------------- --
--  MESSAGE INSTANCES  --
-- ------------------- --

instance FromJSON CallbackMessage where
  parseJSON = withObject "CallbackMessage" $ \o ->
        CallbackMessageText <$> o .: "mid"
                            <*> o .: "text"
                            <*> o .:? "quick_reply"
                            <*> o .:? "seq"
    <|> CallbackMessageSticker <$> o .: "mid"
                               <*> o .: "attachments"
                               <*> o .: "sticker_id"
                               <*> o .:? "seq"
    <|> CallbackMessageAttachment <$> o .: "mid"
                                  <*> o .: "attachments"
                                  <*> o .:? "seq"
    <|> CallbackMessageLocation <$> o .: "mid"
                                <*> o .: "attachments"
                                <*> o .:? "seq"

instance FromJSON CallbackQuickReply where
  parseJSON = withObject "CallbackQuickReply" $ \o -> 
    CallbackQuickReply <$> o .: "payload"

instance FromJSON CallbackAttachment where
  parseJSON = withObject "CallbackAttachment" $ \o ->
    case HM.lookup "type" o of
      Just (String "image") -> CallbackSticker <$> o .: "payload"
      Just (String "template") ->
        CallbackAttachmentTemplate <$> o .: "title"
                                   <*> o .: "subtitle"
                                   <*> o .: "url"
                                   <*> o .: "payload"
      _ -> CallbackAttachment <$> o .: "type"
                              <*> o .: "payload"

instance FromJSON CallbackTemplate where
  parseJSON = withObject "CallbackTemplate" $ \o ->
    case HM.lookup "template_type" o of
      Just (String "generic") ->
        CallbackGenericTemplate <$> o .: "sharable"
                                <*> o .: "elements"
      _ -> fail "Only known template is 'generic' in CallbackTemplate"

instance FromJSON CallbackLocation where
  parseJSON = withObject "CallbackLocation" $ \o ->
    CallbackLocation <$> o .: "title"
                     <*> o .: "url"
                     <*> o .: "payload"

instance FromJSON CallbackMultimediaPayload where
  parseJSON = withObject "CallbackMultimediaPayload" $ \o ->
    CallbackMultimediaPayload <$> o .: "url"

instance FromJSON CallbackStickerPayload where
  parseJSON = withObject "CallbackStickerPayload" $ \o ->
    CallbackStickerPayload <$> o .: "url"
                           <*> o .: "sticker_id"

instance FromJSON CallbackLocationPayload where
  parseJSON = withObject "CallbackLocationPayload" $ \o ->
    CallbackLocationPayload <$> o .: "coordinates" 

instance FromJSON CallbackCoordinates where
  parseJSON = withObject "CallbackCoordinates" $ \o ->
    CallbackCoordinates <$> o .: "lat"
                        <*> o .: "long"


instance ToJSON CallbackMessage where
  toJSON (CallbackMessageText mid text qreply seq') =
    object' [ "mid"         .=! mid
            , "text"        .=! text
            , "quick_reply" .=!! qreply
            , "seq"         .=!! seq'
            ]
  toJSON (CallbackMessageSticker mid attachments sticker_id seq') =
    object' [ "mid"         .=! mid
            , "attachments" .=! attachments
            , "sticker_id"  .=! sticker_id
            , "seq"         .=!! seq'
            ]
  toJSON (CallbackMessageAttachment mid attachments seq') =
    object' [ "mid"         .=! mid
            , "attachments" .=! attachments
            , "seq"         .=!! seq'
            ]
  toJSON (CallbackMessageLocation mid coords seq') =
    object' [ "mid"         .=! mid
            , "attachments" .=! coords
            , "seq"         .=!! seq'
            ]

instance ToJSON CallbackQuickReply where
  toJSON (CallbackQuickReply payload) = object [ "payload" .= payload ]

instance ToJSON CallbackAttachment where
  toJSON (CallbackSticker payload) =
    object [ "type"    .= String "image"
           , "payload" .= payload
           ]
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

instance ToJSON CallbackTemplate where
  toJSON (CallbackGenericTemplate sharable elements) =
    object [ "template_type" .= String "generic"
           , "sharable"      .= sharable
           , "elements"      .= elements
           ]

instance ToJSON CallbackLocation where
  toJSON (CallbackLocation title url payload) =
    object [ "type"    .= String "location"
           , "title"   .= title
           , "url"     .= url
           , "payload" .= payload
           ]

instance ToJSON CallbackMultimediaPayload where
  toJSON (CallbackMultimediaPayload url) = object [ "url" .= url ]

instance ToJSON CallbackStickerPayload where
  toJSON (CallbackStickerPayload url sticker_id) =
    object [ "url"        .= url
           , "sticker_id" .= sticker_id
           ]

instance ToJSON CallbackLocationPayload where
  toJSON (CallbackLocationPayload coords) = object [ "coordinates" .= coords ]

instance ToJSON CallbackCoordinates where
  toJSON (CallbackCoordinates lat long) =
    object [ "lat"  .= lat
           , "long" .= long
           ]