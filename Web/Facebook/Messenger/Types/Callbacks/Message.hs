module Web.Facebook.Messenger.Types.Callbacks.Message where


import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)
import Web.Facebook.Messenger.Types.Static  (AttachmentType)


-- ------------------ --
--  MESSAGE CALLBACK  --
-- ------------------ --

data CallbackMessage =
    CallbackMessageText
        { cb_msg_mid        :: Text -- Message ID
        , cb_msg_seq        :: Int  -- Message sequence number
        , cb_msg_text       :: Text -- Text of message
        , cb_msg_quickreply :: Maybe CallbackQuickReply -- Optional custom data provided by the sending app
        }
  | CallbackMessageAttachment
        { cb_msg_mid         :: Text -- Message ID
        , cb_msg_seq         :: Int  -- Message sequence number
        , cb_msg_attachments :: [CallbackAttachment] -- Array containing attachment data
        }
  deriving (Eq, Show)

newtype CallbackQuickReply = CallbackQuickReply { cb_quick_reply_payload :: Text }
  deriving (Eq, Show)

data CallbackAttachment =
  CallbackMultimediaAttachment
    { cb_attachment_type    :: AttachmentType
    , cb_attachment_payload :: CallbackMultimediaPayload }
  | CallbackCoordinatesAttachment
    { cb_attachment_coordinates :: CallbackCoordinatesAttachmentPayload }
  deriving (Eq, Show)

newtype CallbackMultimediaPayload  =
    CallbackMultimediaPayload { cb_multimedia_payload_url :: Text } -- URL of the file
  deriving (Eq, Show)

newtype CallbackCoordinatesAttachmentPayload =
    CallbackCoordinatesAttachmentPayload { cb_coordinates_payload :: CallbackAttachmentCoordinates }
  deriving (Eq, Show)

data CallbackAttachmentCoordinates = CallbackAttachmentCoordinates
    { cb_coords_lat  :: Double -- Latitude
    , cb_coords_long :: Double -- Longitude
    }
  deriving (Eq, Show)


-- ------------------- --
--  MESSAGE INSTANCES  --
-- ------------------- --

instance FromJSON CallbackMessage where
    parseJSON (Object o) = CallbackMessageText <$> o .: "mid"
                                               <*> o .: "seq"
                                               <*> o .: "text"
                                               <*> o .:? "quick_reply"
                       <|> CallbackMessageAttachment <$> o .: "mid"
                                                     <*> o .: "seq"
                                                     <*> o .: "attachments"
    parseJSON wat = typeMismatch "CallbackMessage" wat

instance FromJSON CallbackQuickReply where
    parseJSON (Object o) = CallbackQuickReply <$> o .: "payload"
    parseJSON wat = typeMismatch "CallbackQuickReply" wat

instance FromJSON CallbackAttachment where
    parseJSON (Object o) = CallbackMultimediaAttachment <$> o .: "type"
                                                        <*> o .: "payload"
                       <|> CallbackCoordinatesAttachment <$> o .: "payload"
    parseJSON wat = typeMismatch "CallbackAttachment" wat

instance FromJSON CallbackMultimediaPayload where
    parseJSON (Object o) = CallbackMultimediaPayload <$> o .: "url"
    parseJSON wat = typeMismatch "CallbackMultimediaPayload" wat

instance FromJSON CallbackCoordinatesAttachmentPayload where
    parseJSON (Object o) = CallbackCoordinatesAttachmentPayload <$> o .: "coordinates" 
    parseJSON wat = typeMismatch "CallbackCoordinatesAttachmentPayload" wat

instance FromJSON CallbackAttachmentCoordinates where
    parseJSON (Object o) = CallbackAttachmentCoordinates <$> o .: "lat"
                                                         <*> o .: "long"
    parseJSON wat = typeMismatch "CallbackAttachmentCoordinates" wat


instance ToJSON CallbackMessage where
    toJSON (CallbackMessageText mid seq' text qreply) = object [ "mid" .= mid
                                                               , "seq" .= seq'
                                                               , "text" .= text
                                                               , "quick_reply" .= qreply
                                                               ]
    toJSON (CallbackMessageAttachment mid seq' attachments) = object [ "mid" .= mid
                                                                     , "seq" .= seq'
                                                                     , "attachments" .= attachments
                                                                     ]

instance ToJSON CallbackQuickReply where
    toJSON (CallbackQuickReply payload) = object [ "payload" .= payload ]

instance ToJSON CallbackAttachment where
    toJSON (CallbackMultimediaAttachment typ payload) = object [ "type" .= typ
                                                               , "payload" .= payload
                                                               ]
    toJSON (CallbackCoordinatesAttachment payload) = object [ "type" .= String "location"
                                                            , "payload" .= payload
                                                            ]

instance ToJSON CallbackMultimediaPayload where
    toJSON (CallbackMultimediaPayload url) = object [ "url" .= url ]

instance ToJSON CallbackCoordinatesAttachmentPayload where
    toJSON (CallbackCoordinatesAttachmentPayload coords) = object [ "coordinates" .= coords ]

instance ToJSON CallbackAttachmentCoordinates where
    toJSON (CallbackAttachmentCoordinates lat long) = object [ "lat" .= lat
                                                             , "long" .= long
                                                             ]