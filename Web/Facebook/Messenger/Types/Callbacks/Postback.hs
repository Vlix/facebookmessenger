{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.Postback
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Postbacks occur when a `PostbackButton`, `GetStartedButton`, or `PersistentMenuItem` is tapped.
The payload field passed is defined in the above places.

You can subscribe to this callback by selecting the @"messaging_postbacks"@ field when setting up your webhook.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_postbacks
-}
module Web.Facebook.Messenger.Types.Callbacks.Postback (
  -- * Postback Callback
  Postback (..)
  )
where


import Data.Text (Text)
import Data.Aeson

import Web.Facebook.Messenger.Types.Callbacks.Referral (Referral)
import Web.Facebook.Messenger.Types.Requests.Extra (PostbackButton)
import Web.Facebook.Messenger.Types.Requests.Settings (GetStartedButton, PersistentMenuItem)
import Web.Facebook.Messenger.Types.Static


-- ------------------- --
--  POSTBACK CALLBACK  --
-- ------------------- --

-- | Postbacks occur when a `PostbackButton`, `GetStartedButton`, or `PersistentMenuItem` is tapped.
-- The payload field passed is defined in the above places.
data Postback = Postback
    { pbTitle :: Text
    -- ^ Title for the CTA that was clicked on. This is sent to all apps subscribed to the page.
    -- For apps other than the original CTA sender, the postback event will be delivered via the standby channel.
    , pbPayload :: Text
    -- ^ Payload parameter that was defined with the button.
    -- This is only visible to the app that send the original template message.
    , pbReferral :: Maybe Referral
    -- ^ This section is present only if:
    --
    -- * The user entered the thread via an @m.me@ link with a ref parameter and tapped the Get Started button.
    -- * The user entered the thread by scanning a parametric Messenger Code and tapped the Get Started button.
    -- * This is the first postback after user came from a Messenger Conversation Ad.
    -- * The user entered the thread via Discover tab and tapped the Get Started button.
    --
    -- https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_referrals
    } deriving (Eq, Show)


-- -------------------- --
--  POSTBACK INSTANCES  --
-- -------------------- --

instance ToJSON Postback where
  toJSON (Postback title payload ref) =
      object' [ "title" .=! title
              , "payload" .=! payload
              , "referral" .=!! ref
              ]

instance FromJSON Postback where
  parseJSON = withObject "Postback" $ \o ->
        Postback <$> o .: "title"
                 <*> o .: "payload"
                 <*> o .:? "referral"
