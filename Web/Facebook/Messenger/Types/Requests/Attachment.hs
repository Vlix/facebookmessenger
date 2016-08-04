module Web.Facebook.Messenger.Types.Requests.Attachment
    ( FBRequestAttachment (..)
    , FBRequestMultimediaPayload (..)
    , module Web.Facebook.Messenger.Types.Requests.Templates
    ) where

import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)

import Web.Facebook.Messenger.Types.Requests.Templates
import Web.Facebook.Messenger.Types.Static


-- -------------------- --
--  ATTACHMENT REQUEST  --
-- -------------------- --

data FBRequestAttachment =
  FBRequestMultimediaAttachment
    { fbreq_attachment_type               :: FBRequestAttachmentType    -- IMAGE, AUDIO, VIDEO, or FILE
    , fbreq_attachment_multimedia_payload :: FBRequestMultimediaPayload
    }
  | FBRequestAttachmentTemplate
    { fbreq_attachment_template_payload :: FBRequestTemplatePayload }
  deriving (Eq, Show)

newtype FBRequestMultimediaPayload =
    FBRequestMultimediaPayload { fbreq_multimedia_payload_url :: Text } -- URL of payload
  deriving (Eq, Show)


-- ---------------------- --
--  ATTACHMENT INSTANCES  --
-- ---------------------- --

instance ToJSON FBRequestAttachment where
    toJSON (FBRequestMultimediaAttachment typ payload) = object [ "type" .= typ
                                                                , "payload" .= payload ]
    toJSON (FBRequestAttachmentTemplate payload) = object [ "type" .= String "template"
                                                          , "payload" .= payload ]

instance ToJSON FBRequestMultimediaPayload where
    toJSON (FBRequestMultimediaPayload url) = object [ "url" .= url ]


instance FromJSON FBRequestAttachment where
    parseJSON (Object o) = FBRequestMultimediaAttachment <$> o .: "type"
                                                         <*> o .: "payload"
                       <|> FBRequestAttachmentTemplate <$> o .: "payload"
    parseJSON wat = typeMismatch "FBRequestAttachment" wat

instance FromJSON FBRequestMultimediaPayload where
    parseJSON (Object o) = FBRequestMultimediaPayload <$> o .: "url"
    parseJSON wat = typeMismatch "FBRequestMultimediaPayload" wat
