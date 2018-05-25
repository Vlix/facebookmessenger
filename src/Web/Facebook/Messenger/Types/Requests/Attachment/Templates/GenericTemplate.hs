{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment.Templates.GenericTemplate
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Use the `GenericTemplate` with the Send API to send a horizontal scrollable carousel of items,
each composed of an image attachment, short description and buttons to request input from the user.

Buttons in `GenericTemplate` can do the following:

* Open a URL
* Make a postback to your webhook
* Call a phone number
* Open a share dialog
* Open a payment dialog
* Log in or out of an external service

https://developers.facebook.com/docs/messenger-platform/send-api-reference/generic-template
-}
module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.GenericTemplate (
  -- * Generic Template
  GenericTemplate (..)
  )
where

import Data.Aeson
import Data.List.NonEmpty as NE (NonEmpty, take)
import Data.Text (Text)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests.Extra (GenericElement)
import Web.Facebook.Messenger.Types.Static


-- -------------------------- --
--  GENERIC TEMPLATE REQUEST  --
-- -------------------------- --

-- | The `GenericElement` type and instances are in the "Extra" module because
-- the `ShareButton` depends on it, and the `GenericElement` depends on `TemplateButton`
data GenericTemplate = GenericTemplate
    { gtSharable :: Bool
    -- ^ Set to false to disable the native share button in Messenger for the template message.
    -- (Though I think the default is False)
    , gtImageAspectRatio :: ImageAspectRatioType
    -- ^ Aspect ratio used to render images specified by image_url in element objects.
    -- Must be `HORIZONTAL` or `SQUARE`. Default is `HORIZONTAL`.
    , gtElements :: NonEmpty GenericElement -- ^ Data for each bubble in message (Limited to 10)
    } deriving (Eq, Show, Read, Ord)

instance ToJSON GenericTemplate where
  toJSON (GenericTemplate share ratio elements) =
      object' [ "template_type" .=! String "generic"
              , mDefault "sharable" True share
              , mDefault "image_aspect_ratio" HORIZONTAL ratio
              , "elements" .=! NE.take 10 elements
              ]

instance FromJSON GenericTemplate where
  parseJSON = checkValue
      "GenericTemplate"
      "template_type"
      ("generic" :: Text)
      $ \o -> GenericTemplate <$> o .:? "sharable" .!= True
                              <*> o .:? "image_aspect_ratio" .!= HORIZONTAL
                              <*> o .: "elements"
