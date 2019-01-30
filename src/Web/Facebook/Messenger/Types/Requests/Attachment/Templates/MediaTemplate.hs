{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment.Templates.MediaTemplate
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

The 'MediaTemplate' allows you to send images, GIFs, and video
as a structured message with an optional button.
Videos and animated GIFs sent with the media template are playable in the conversation.

https://developers.facebook.com/docs/messenger-platform/send-messages/template/media
-}
module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.MediaTemplate (
  -- * Media Template
  MediaTemplate (..)
  , MediaElement (..)
  , MediaElementContent (..)
  )
where

import Data.Aeson
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests.Extra (TemplateButton)
import Web.Facebook.Messenger.Types.Static


-- ----------------------- --
--  LIST TEMPLATE REQUEST  --
-- ----------------------- --

-- | Template for sending media.
--
-- (Currently, the media template only supports sending images and video. Audio is currently not supported)
newtype MediaTemplate = MediaTemplate { mtElements :: MediaElement }
  deriving (Eq, Show, Read, Ord)

instance ToJSON MediaTemplate where
  toJSON (MediaTemplate element) =
      object' [ "template_type" .=! String "media"
              , "elements" .=! [element]
              ]

instance FromJSON MediaTemplate where
  parseJSON = checkValue
      "MediaTemplate"
      "template_type"
      (String "media")
      mkMediaTemplate
    where mkMediaTemplate o = do
            el <- o .: "elements"
            case el of
              [e] -> pure $ MediaTemplate e
              _ -> fail "not just one media template element"

-- | Elements used in the 'MediaTemplate'
data MediaElement = MediaElement
    { meType :: AttachmentType -- ^ Type of the media (can only be 'VIDEO' or 'IMAGE')
    , meContent :: MediaElementContent -- ^ Is either an @Attachment ID@ or a @Facebook URL@
    , meButton :: Maybe TemplateButton -- ^ Buttons on the element (1 button limit)
    } deriving (Eq, Show, Read, Ord)

instance ToJSON MediaElement where
  toJSON (MediaElement typ content mBtn) =
      object' [ "media_type" .=! typ
              , contentPair
              , mEmptyList "buttons" $ btnList mBtn
              ]
    where btnList Nothing = []
          btnList (Just btn) = [btn]
          contentPair = case content of
                          AttachmentID attID -> "attachment_id" .=! attID
                          FacebookURL url -> "url" .=! url

instance FromJSON MediaElement where
  parseJSON = withObject "MediaElement" $ \o -> do
      mURL <- o .:? "url"
      mAttID <- o .:? "attachment_id"
      content <- case (mURL, mAttID) of
                  (Nothing, Just attID) -> return $ AttachmentID attID
                  (Just url, Nothing) -> return $ FacebookURL url
                  _ -> fail "object contains both \"url\" and \"attachment_id\""
      MediaElement <$> o .: "media_type"
                   <*> pure content
                   <*> (listToMaybe <$> o .:? "buttons" .!= [])

-- | Content of a 'MediaElement'. Is either an attachment id, or the URL to a
-- Facebook page's or Facebook account's media.
data MediaElementContent =
      AttachmentID Text
      -- ^ ID received when uploading attachment to attachment API
      -- or when setting "is_reusable" when sending an attachment message
    | FacebookURL URL
      -- ^ Getting the Facebook URL
      --
      -- To get the Facebook URL for an image or video, do the following:
      --
      -- * Click the image or video thumbnail to open the full-size view.
      -- * Copy the URL from your browser's address bar.
      --
      -- Facebook URLs should be in the following base format:
      --
      -- @
      -- Media Type  Media Source      URL Format
      --
      -- Video       Facebook Page     https://business.facebook.com/<PAGE_NAME>/videos/<NUMERIC_ID>
      --
      -- Video       Facebook Account  https://www.facebook.com/<USERNAME>/videos/<NUMERIC_ID>/
      --
      -- Image       Facebook Page     https://business.facebook.com/<PAGE_NAME>/photos/<NUMERIC_ID>
      --
      -- Image       Facebook Account  https://www.facebook.com/photo.php?fbid=<NUMERIC_ID>
      -- @

  deriving (Eq, Show, Read, Ord)
