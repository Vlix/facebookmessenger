module Web.Facebook.Messenger.Types.Requests.Attachment
    ( RequestAttachment (..)
    , RequestMultimediaPayload (..)
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

data RequestAttachment =
  RequestMultimediaAttachment
    { req_attachment_type               :: AttachmentType    -- IMAGE, AUDIO, VIDEO, or FILE
    , req_attachment_multimedia_payload :: RequestMultimediaPayload
    }
  | RequestAttachmentTemplate
    { req_attachment_template_payload :: TemplatePayload }
  deriving (Eq, Show)

newtype RequestMultimediaPayload =
    RequestMultimediaPayload { req_multimedia_payload_url :: Text } -- URL of payload
  deriving (Eq, Show)


-- ---------------------- --
--  ATTACHMENT INSTANCES  --
-- ---------------------- --

instance ToJSON RequestAttachment where
    toJSON (RequestMultimediaAttachment typ payload) = object [ "type" .= typ
                                                              , "payload" .= payload ]
    toJSON (RequestAttachmentTemplate payload) = object [ "type" .= String "template"
                                                        , "payload" .= payload ]

instance ToJSON RequestMultimediaPayload where
    toJSON (RequestMultimediaPayload url) = object [ "url" .= url ]


instance FromJSON RequestAttachment where
    parseJSON (Object o) = RequestMultimediaAttachment <$> o .: "type"
                                                       <*> o .: "payload"
                       <|> RequestAttachmentTemplate <$> o .: "payload"
    parseJSON wat = typeMismatch "RequestAttachment" wat

instance FromJSON RequestMultimediaPayload where
    parseJSON (Object o) = RequestMultimediaPayload <$> o .: "url"
    parseJSON wat = typeMismatch "RequestMultimediaPayload" wat
