{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental


-}
module Web.Facebook.Messenger.Types.Requests.Attachment
    ( RequestAttachment (..)
    , templateRequest
    , multimediaRequest
    , reusedMultimediaRequest
    , module Web.Facebook.Messenger.Types.Requests.Templates
    ) where

import Control.Applicative ((<|>))
import Data.Text
import Data.Aeson

import Web.Facebook.Messenger.Types.Requests.Templates
import Web.Facebook.Messenger.Types.Static


templateRequest :: TemplatePayload -> RequestAttachment
templateRequest = RTemplate . RequestAttachmentTemplate

multimediaRequest :: AttachmentType -> URL -> Bool -> RequestAttachment
multimediaRequest typ url =
    RMultimedia . RequestMultimediaAttachment typ
                . RMPayload
                . RMultimediaPayload url

reusedMultimediaRequest :: AttachmentType -> Text -> RequestAttachment
reusedMultimediaRequest typ =
    RMultimedia . RequestMultimediaAttachment typ
                . RMReused
                . RReusedMultimediaPayload


-- -------------------- --
--  ATTACHMENT REQUEST  --
-- -------------------- --


data RequestAttachment = RMultimedia RequestMultimediaAttachment
                       | RTemplate RequestAttachmentTemplate
  deriving (Eq, Show)

data RequestMultimediaAttachment = RequestMultimediaAttachment
    { rmaType :: AttachmentType -- IMAGE, AUDIO, VIDEO, or FILE
    , rmaPayload :: RequestMultimediaPayload -- Attachments sent with the Send API can be up to 25 MB.
    } deriving (Eq, Show)

newtype RequestAttachmentTemplate =
          RequestAttachmentTemplate {ratPayload :: TemplatePayload}
  deriving (Eq, Show)

data RequestMultimediaPayload = RMPayload RMultimediaPayload
                              | RMReused RReusedMultimediaPayload
  deriving (Eq, Show)

data RMultimediaPayload = RMultimediaPayload
      { rmpUrl :: URL -- URL of payload
      , rmpIsReusable :: Bool -- Makes resending attachments easier
      } deriving (Eq, Show)

newtype RReusedMultimediaPayload =
          RReusedMultimediaPayload {rrmpId :: Text} -- ID of the reusable attachment
  deriving (Eq, Show)


-- ---------------------- --
--  ATTACHMENT INSTANCES  --
-- ---------------------- --

instance ToJSON RequestAttachment where
  toJSON (RMultimedia x) = toJSON x
  toJSON (RTemplate x) = toJSON x

instance ToJSON RequestMultimediaAttachment where
  toJSON (RequestMultimediaAttachment typ payload) =
      object [ "type" .= typ
             , "payload" .= payload ]

instance ToJSON RequestAttachmentTemplate where
  toJSON (RequestAttachmentTemplate payload) =
      object [ "type" .= String "template"
             , "payload" .= payload ]

instance ToJSON RequestMultimediaPayload where
  toJSON (RMPayload x) = toJSON x
  toJSON (RMReused x) = toJSON x

instance ToJSON RMultimediaPayload where
  toJSON (RMultimediaPayload url reuse) =
      object' [ "url" .=! url
              , mDefault "is_reusable" False reuse
              ]

instance ToJSON RReusedMultimediaPayload where
  toJSON (RReusedMultimediaPayload ident) =
      object [ "attachment_id" .= ident ]

instance FromJSON RequestAttachment where
  parseJSON = withObject "RequestAttachment" $ \o ->
        RMultimedia <$> parseJSON (Object o)
    <|> RTemplate <$> parseJSON (Object o)

instance FromJSON RequestMultimediaAttachment where
  parseJSON = withObject "RequestMultimediaAttachment" $ \o ->
      RequestMultimediaAttachment <$> o .: "type"
                                  <*> o .: "payload"

instance FromJSON RequestAttachmentTemplate where
  parseJSON = withObject "RequestAttachmentTemplate" $ \o ->
      RequestAttachmentTemplate <$> o .: "payload"

instance FromJSON RequestMultimediaPayload where
  parseJSON = withObject "RequestMultimediaPayload" $ \o ->
        RMPayload <$> parseJSON (Object o)
    <|> RMReused <$> parseJSON (Object o)

instance FromJSON RMultimediaPayload where
  parseJSON = withObject "RMultimediaPayload" $ \o ->
      RMultimediaPayload <$> o .: "url"
                         <*> o .:? "is_reusable" .!= False

instance FromJSON RReusedMultimediaPayload where
  parseJSON = withObject "RReusedMultimediaPayload" $ \o ->
      RReusedMultimediaPayload <$> o .: "attachment_id"
