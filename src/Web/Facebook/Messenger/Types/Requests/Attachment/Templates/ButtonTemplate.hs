{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment.Templates.ButtonTemplate
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Use the Button Template with the Send API to send a text and
buttons attachment to request input from the user.
The buttons can open a URL, or make a back-end call to your webhook.

https://developers.facebook.com/docs/messenger-platform/send-api-reference/button-template
-}
module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.ButtonTemplate (
  -- * Button Template
  ButtonTemplate (..)
  )
where

import Data.Aeson
import Data.List.NonEmpty as NE (NonEmpty, take)
import Data.Text (Text)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests.Extra (TemplateButton)


-- ------------------------- --
--  BUTTON TEMPLATE REQUEST  --
-- ------------------------- --

-- | Template for sending a bubble with text and 1-3 buttons to a user
data ButtonTemplate = ButtonTemplate
    { btText :: Text
    -- ^ UTF-8-encoded text of up to 640 characters that appears above the buttons
    , btButtons :: NonEmpty TemplateButton -- ^ Set of 1-3 buttons that appear as call-to-actions
    } deriving (Eq, Show, Read, Ord)

instance ToJSON ButtonTemplate where
  toJSON (ButtonTemplate text buttons) =
      object [ "template_type" .= String "button"
             , "text" .= text
             , "buttons" .= NE.take 3 buttons
             ]

instance FromJSON ButtonTemplate where
  parseJSON = checkValue
      "ButtonTemplate"
      "template_type"
      ("button" :: Text)
      $ \o -> ButtonTemplate <$> o .: "text"
                             <*> o .: "buttons"
