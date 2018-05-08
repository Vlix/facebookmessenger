{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Attachment types used in the Send API

https://developers.facebook.com/docs/messenger-platform/send-api-reference/contenttypes
-}
module Web.Facebook.Messenger.Types.Requests.Attachment (
    -- * Send API Attachment
    RequestAttachment (..)
    -- ** Multimedia
    , multimediaRequest
    , reusedMultimediaRequest
    , RequestMultimediaAttachment (..)
    , RequestAttachmentTemplate (..)
    , RequestMultimediaPayload (..)
    , RMultimediaPayload (..)
    , RReusedMultimediaPayload (..)
    -- ** Template
    , templateRequest
    , buttonTemplate
    , genericTemplate
    , genericTemplate_
    , listTemplate
    , listTemplate_
    , mediaTemplate
    , openGraphTemplate
    -- * Exported Modules
    , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates
    ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Text

import Web.Facebook.Messenger.Types.Requests.Attachment.Templates
import Web.Facebook.Messenger.Types.Static

-- | Constructor for making a `RequestAttachment` from a `TemplatePayload`
templateRequest :: TemplatePayload -> RequestAttachment
templateRequest = RTemplate . RequestAttachmentTemplate

-- | Shortcut constructor for making a `ButtonTemplate` `RequestAttachment`
buttonTemplate :: Text -- ^ /UTF-8-encoded text of up to 640 characters that appears above the buttons/
               -> NonEmpty TemplateButton -- ^ /Set of 1-3 buttons that appear as call-to-actions/
               -> RequestAttachment
buttonTemplate = (templateRequest .) . buttonTemplateP

-- | Constructor for making a `GenericTemplate` `RequestAttachment`
genericTemplate :: Bool
                -- ^ /Set to `False` to disable the native share button in Messenger for the template message./
                -- /(Though I think the default is False)/
                -> ImageAspectRatioType
                -- ^ /Aspect ratio used to render images specified by image_url in element objects./
                -- /Must be `HORIZONTAL` or `SQUARE`. Default is `HORIZONTAL`./
                -> NonEmpty GenericElement -- ^ /Data for each bubble in message (Limited to 10)/
                -> RequestAttachment
genericTemplate = ((templateRequest .) .) . genericTemplateP

-- | Shortcut for a default `GenericTemplate` `RequestAttachment`
--
-- @genericTemplate_ = genericTemplate True HORIZONTAL@
genericTemplate_ :: NonEmpty GenericElement -> RequestAttachment
genericTemplate_ = genericTemplate True HORIZONTAL


-- | Constructor for making a `ListTemplate` `RequestAttachment`
listTemplate :: ListStyle -> NonEmpty ListElement -> Maybe TemplateButton -> RequestAttachment
listTemplate lStyle es = templateRequest . listTemplateP lStyle es

-- | Shortcut for a simple default `ListTemplate` `RequestAttachment`
listTemplate_ :: NonEmpty ListElement -> RequestAttachment
listTemplate_ = flip (listTemplate ListLARGE) Nothing

-- | Constructor for making a 'MediaTemplate' 'RequestAttachment'
mediaTemplate :: MediaElement -> RequestAttachment
mediaTemplate = templateRequest . mediaTemplateP

-- | Constructor for making a `OpenGraphTemplate` `RequestAttachment`
openGraphTemplate :: URL -> [TemplateButton] -> RequestAttachment
openGraphTemplate = (templateRequest .) . openGraphTemplateP

-- | Constructor for a Multimedia `RequestAttachment`
multimediaRequest :: AttachmentType -- ^ `IMAGE` \/ `AUDIO` \/ `VIDEO` \/ `FILE`
                  -> URL -- ^ URL to the media
                  -> Bool -- ^ Whether to receive a reusable @"attachment_id"@
                  -> RequestAttachment
multimediaRequest typ url =
    RMultimedia . RequestMultimediaAttachment typ
                . RMPayload
                . RMultimediaPayload url

-- | Constructor for a reusable Multimedia `RequestAttachment`
reusedMultimediaRequest :: AttachmentType -- ^ `IMAGE` \/ `AUDIO` \/ `VIDEO` \/ `FILE`
                        -> Text -- ^ The @"attachment_id"@ of the reusable media
                        -> RequestAttachment
reusedMultimediaRequest typ =
    RMultimedia . RequestMultimediaAttachment typ
                . RMReused
                . RReusedMultimediaPayload


-- -------------------- --
--  ATTACHMENT REQUEST  --
-- -------------------- --

-- | Send a multimedia attachment or a structed template to a user
data RequestAttachment = RMultimedia RequestMultimediaAttachment
                       | RTemplate RequestAttachmentTemplate
  deriving (Eq, Show, Read, Ord)

-- | Multimedia Attachment. Sends a piece of media to a user.
--
-- /Attachments sent with the Send API used to have a 25 MB limit/
data RequestMultimediaAttachment = RequestMultimediaAttachment
    { rmaType :: AttachmentType -- ^ `IMAGE` \/ `AUDIO` \/ `VIDEO` \/ `FILE`
    , rmaPayload :: RequestMultimediaPayload -- ^
    } deriving (Eq, Show, Read, Ord)

-- | Wrapper around `TemplatePayload` for @JSON@ reasons
newtype RequestAttachmentTemplate =
          RequestAttachmentTemplate {ratPayload :: TemplatePayload}
  deriving (Eq, Show, Read, Ord)

-- | What kind of multimedia payload. `RMReused` will need an @"attachment_id"@ you need to receive from Facebook first.
data RequestMultimediaPayload = RMPayload RMultimediaPayload
                              | RMReused RReusedMultimediaPayload
  deriving (Eq, Show, Read, Ord)

-- | Multimedia payload
data RMultimediaPayload = RMultimediaPayload
      { rmpUrl :: URL -- ^ URL of media
      , rmpIsReusable :: Bool -- ^ When `True`, Facebook will respond with an @"attachment_id"@
      } deriving (Eq, Show, Read, Ord)

-- | Reusable multimedia. Text is @"attachment_id"@ of the reusable attachment
newtype RReusedMultimediaPayload =
          RReusedMultimediaPayload {rrmpId :: Text}
  deriving (Eq, Show, Read, Ord)


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
