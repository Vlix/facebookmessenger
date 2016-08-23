module Web.Facebook.Messenger.Types.Callbacks.Message where


import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)
import Web.Facebook.Messenger.Types.Static  (FBAttachmentType)


-- ------------------ --
--  MESSAGE CALLBACK  --
-- ------------------ --

data FBCallbackMessage =
    FBCallbackMessage
        { fbcb_msg_mid        :: Text -- Message ID
        , fbcb_msg_seq        :: Int  -- Message sequence number
        , fbcb_msg_text       :: Text -- Text of message
        , fbcb_msg_quickreply :: Maybe FBCallbackQuickReply -- Optional custom data provided by the sending app
        }
  | FBCallbackAttachment
        { fbcb_msg_mid         :: Text -- Message ID
        , fbcb_msg_seq         :: Int  -- Message sequence number
        , fbcb_msg_attachments :: [FBCallbackAttachment] -- Array containing attachment data
        }
  deriving (Eq, Show)

newtype FBCallbackQuickReply = FBCallbackQuickReply { fbcb_quick_reply_payload :: Text }
  deriving (Eq, Show)

data FBCallbackAttachment =
  FBCallbackMultimediaAttachment
    { fbcb_attachment_type    :: FBAttachmentType
    , fbcb_attachment_payload :: FBCallbackMultimediaAttachmentPayload }
  | FBCallbackCoordinatesAttachment
    { fbcb_attachment_coordinates :: FBCallbackCoordinatesAttachmentPayload }
  deriving (Eq, Show)

newtype FBCallbackMultimediaAttachmentPayload  =
    FBCallbackMultimediaAttachmentPayload { fbcb_multimedia_payload_url :: Text } -- URL of the file
  deriving (Eq, Show)

newtype FBCallbackCoordinatesAttachmentPayload =
    FBCallbackCoordinatesAttachmentPayload { fbcb_coordinates_payload :: FBCallbackAttachmentCoordinates }
  deriving (Eq, Show)

data FBCallbackAttachmentCoordinates = FBCallbackAttachmentCoordinates
    { fbcb_coords_lat  :: Double -- Latitude
    , fbcb_coords_long :: Double -- Longitude
    }
  deriving (Eq, Show)


-- ------------------- --
--  MESSAGE INSTANCES  --
-- ------------------- --

instance FromJSON FBCallbackMessage where
    parseJSON (Object o) = FBCallbackMessage <$> o .: "mid"
                                             <*> o .: "seq"
                                             <*> o .: "text"
                                             <*> o .:? "quick_reply"
                       <|> FBCallbackAttachment <$> o .: "mid"
                                                <*> o .: "seq"
                                                <*> o .: "attachments"
    parseJSON wat = typeMismatch "FBCallbackMessage" wat

instance FromJSON FBCallbackQuickReply where
    parseJSON (Object o) = FBCallbackQuickReply <$> o .: "payload"
    parseJSON wat = typeMismatch "FBCallbackQuickReply" wat

instance FromJSON FBCallbackAttachment where
    parseJSON (Object o) = FBCallbackMultimediaAttachment <$> o .: "type"
                                                          <*> o .: "payload"
                       <|> FBCallbackCoordinatesAttachment <$> o .: "payload"
    parseJSON wat = typeMismatch "FBCallbackAttachment" wat

instance FromJSON FBCallbackMultimediaAttachmentPayload where
    parseJSON (Object o) = FBCallbackMultimediaAttachmentPayload <$> o .: "url"
    parseJSON wat = typeMismatch "FBCallbackMultimediaAttachmentPayload" wat

instance FromJSON FBCallbackCoordinatesAttachmentPayload where
    parseJSON (Object o) = FBCallbackCoordinatesAttachmentPayload <$> o .: "coordinates" 
    parseJSON wat = typeMismatch "FBCallbackCoordinatesAttachmentPayload" wat

instance FromJSON FBCallbackAttachmentCoordinates where
    parseJSON (Object o) = FBCallbackAttachmentCoordinates <$> o .: "lat"
                                                           <*> o .: "long"
    parseJSON wat = typeMismatch "FBCallbackAttachmentCoordinates" wat


instance ToJSON FBCallbackMessage where
    toJSON (FBCallbackMessage mid seq' text qreply) = object [ "mid" .= mid
                                                             , "seq" .= seq'
                                                             , "text" .= text
                                                             , "quick_reply" .= qreply
                                                             ]
    toJSON (FBCallbackAttachment mid seq' attachments) = object [ "mid" .= mid
                                                                , "seq" .= seq'
                                                                , "attachments" .= attachments
                                                                ]

instance ToJSON FBCallbackQuickReply where
    toJSON (FBCallbackQuickReply payload) = object [ "payload" .= payload ]

instance ToJSON FBCallbackAttachment where
    toJSON (FBCallbackMultimediaAttachment typ payload) = object [ "type" .= typ
                                                                 , "payload" .= payload
                                                                 ]
    toJSON (FBCallbackCoordinatesAttachment payload) = object [ "type" .= String "location"
                                                              , "payload" .= payload
                                                              ]

instance ToJSON FBCallbackMultimediaAttachmentPayload where
    toJSON (FBCallbackMultimediaAttachmentPayload url) = object [ "url" .= url ]

instance ToJSON FBCallbackCoordinatesAttachmentPayload where
    toJSON (FBCallbackCoordinatesAttachmentPayload coords) = object [ "coordinates" .= coords ]

instance ToJSON FBCallbackAttachmentCoordinates where
    toJSON (FBCallbackAttachmentCoordinates lat long) = object [ "lat" .= lat
                                                               , "long" .= long
                                                               ]