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
  | CallbackMessageLocation
        { cb_msg_mid    :: Text -- Message ID
        , cb_msg_seq    :: Int  -- Message sequence number
        , cb_msg_coords :: [CallbackLocation] -- Array containing Location Quick Reply Callback (probably just 1)
        }
  deriving (Eq, Show)

newtype CallbackQuickReply = CallbackQuickReply { cb_quick_reply_payload :: Text }
  deriving (Eq, Show)

data CallbackAttachment = CallbackAttachment
    { cb_attachment_type    :: AttachmentType
    , cb_attachment_payload :: CallbackMultimediaPayload }
  deriving (Eq, Show)

data CallbackLocation = CallbackLocation
    { cb_attachment_title  :: Text
    , cb_attachment_url    :: Text
    , cb_attachment_coords :: CallbackLocationPayload }
  deriving (Eq, Show)

newtype CallbackMultimediaPayload  =
    CallbackMultimediaPayload { cb_multimedia_payload_url :: Text } -- URL of the file
  deriving (Eq, Show)

newtype CallbackLocationPayload =
    CallbackLocationPayload { cb_coordinates_payload :: CallbackCoordinates }
  deriving (Eq, Show)

data CallbackCoordinates = CallbackCoordinates
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
                       <|> CallbackMessageLocation <$> o .: "mid"
                                                   <*> o .: "seq"
                                                   <*> o .: "attachments"
    parseJSON wat = typeMismatch "CallbackMessage" wat

instance FromJSON CallbackQuickReply where
    parseJSON (Object o) = CallbackQuickReply <$> o .: "payload"
    parseJSON wat = typeMismatch "CallbackQuickReply" wat

instance FromJSON CallbackAttachment where
    parseJSON (Object o) = CallbackAttachment <$> o .: "type"
                                              <*> o .: "payload"
    parseJSON wat = typeMismatch "CallbackAttachment" wat

instance FromJSON CallbackLocation where
    parseJSON (Object o) = CallbackLocation <$> o .: "title"
                                            <*> o .: "url"
                                            <*> o .: "payload"
    parseJSON wat = typeMismatch "CallbackLocation" wat

instance FromJSON CallbackMultimediaPayload where
    parseJSON (Object o) = CallbackMultimediaPayload <$> o .: "url"
    parseJSON wat = typeMismatch "CallbackMultimediaPayload" wat

instance FromJSON CallbackLocationPayload where
    parseJSON (Object o) = CallbackLocationPayload <$> o .: "coordinates" 
    parseJSON wat = typeMismatch "CallbackLocationPayload" wat

instance FromJSON CallbackCoordinates where
    parseJSON (Object o) = CallbackCoordinates <$> o .: "lat"
                                               <*> o .: "long"
    parseJSON wat = typeMismatch "CallbackCoordinates" wat


instance ToJSON CallbackMessage where
    toJSON (CallbackMessageText mid seq' text qreply) = object [ "mid"         .= mid
                                                               , "seq"         .= seq'
                                                               , "text"        .= text
                                                               , "quick_reply" .= qreply
                                                               ]
    toJSON (CallbackMessageAttachment mid seq' attachments) = object [ "mid"         .= mid
                                                                     , "seq"         .= seq'
                                                                     , "attachments" .= attachments
                                                                     ]

instance ToJSON CallbackQuickReply where
    toJSON (CallbackQuickReply payload) = object [ "payload" .= payload ]

instance ToJSON CallbackAttachment where
    toJSON (CallbackAttachment typ payload) = object [ "type"    .= typ
                                                     , "payload" .= payload
                                                     ]
instance ToJSON CallbackLocation where
    toJSON (CallbackLocation title url payload) = object [ "type"    .= String "location"
                                                         , "title"   .= title
                                                         , "url"     .= url
                                                         , "payload" .= payload
                                                         ]

instance ToJSON CallbackMultimediaPayload where
    toJSON (CallbackMultimediaPayload url) = object [ "url" .= url ]

instance ToJSON CallbackLocationPayload where
    toJSON (CallbackLocationPayload coords) = object [ "coordinates" .= coords ]

instance ToJSON CallbackCoordinates where
    toJSON (CallbackCoordinates lat long) = object [ "lat" .= lat
                                                   , "long" .= long
                                                   ]