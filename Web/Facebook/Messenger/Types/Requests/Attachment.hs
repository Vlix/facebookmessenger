module Web.Facebook.Messenger.Types.Requests.Attachment
    ( RequestAttachment (..)
    , RequestMultimediaPayload (..)
    , module Web.Facebook.Messenger.Types.Requests.Templates
    ) where

import           Control.Applicative  ((<|>))
import           Data.Text
import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)

import           Web.Facebook.Messenger.Types.Requests.Templates
import           Web.Facebook.Messenger.Types.Static


-- -------------------- --
--  ATTACHMENT REQUEST  --
-- -------------------- --

data RequestAttachment =
  RequestMultimediaAttachment
    { req_attachment_type               :: AttachmentType    -- IMAGE, AUDIO, VIDEO, or FILE
    , req_attachment_multimedia_payload :: RequestMultimediaPayload -- Attachments sent with the Send API can be up to 25 MB.
    }
  | RequestAttachmentTemplate
    { req_attachment_template_payload :: TemplatePayload }
  deriving (Eq, Show)

data RequestMultimediaPayload =
    RequestMultimediaPayload
      { req_multimedia_payload_url         :: Text -- URL of payload
      , req_multimedia_payload_is_reusable :: Bool -- Makes resending attachments easier
      }
  | RequestReusedMultimediaPayload
      { req_reused_attachment_id :: Text } -- ID of the reusable attachment
  deriving (Eq, Show)


-- ---------------------- --
--  ATTACHMENT INSTANCES  --
-- ---------------------- --

instance ToJSON RequestAttachment where
  toJSON (RequestMultimediaAttachment typ payload) =
    object [ "type"    .= typ
           , "payload" .= payload ]
  toJSON (RequestAttachmentTemplate payload) =
    object [ "type"    .= String "template"
           , "payload" .= payload ]

instance ToJSON RequestMultimediaPayload where
  toJSON (RequestMultimediaPayload url reuse) =
    object $ [ "url" .= url]
      `mappend` mBool "is_reusable" False reuse
  toJSON (RequestReusedMultimediaPayload ident) =
    object [ "attachment_id" .= ident ]

instance FromJSON RequestAttachment where
  parseJSON (Object o) =
    RequestMultimediaAttachment <$> o .: "type"
                                <*> o .: "payload"
    <|> RequestAttachmentTemplate <$> o .: "payload"
  parseJSON wat = typeMismatch "RequestAttachment" wat

instance FromJSON RequestMultimediaPayload where
  parseJSON (Object o) =
    RequestMultimediaPayload <$> o .: "url"
                             <*> o .:? "is_reusable" .!= False
    <|> RequestReusedMultimediaPayload <$> o .: "attachment_id"
  parseJSON wat = typeMismatch "RequestMultimediaPayload" wat
