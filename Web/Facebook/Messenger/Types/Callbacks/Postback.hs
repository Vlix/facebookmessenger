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
  , RegularPostback (..)
  , SecondaryPostback (..)
  )
where


import Data.Text (Text)
import Data.Aeson

import Web.Facebook.Messenger.Types.Callbacks.Referral (Referral)
import Web.Facebook.Messenger.Types.Static


-- ------------------- --
--  POSTBACK CALLBACK  --
-- ------------------- --

-- | Postbacks occur when a `PostbackButton`, `GetStartedButton`, or `PersistentMenuItem` is tapped.
-- The payload field passed is defined in the above places.
data Postback = PBRegular RegularPostback
              | PBSecondary SecondaryPostback
  deriving (Eq, Show)

-- | This postback has a payload included, meaning it's from the app that receives this is the app that sent it.
data RegularPostback = RegularPostback
    { rpTitle :: Text
    -- ^ Title for the CTA that was clicked on. This is sent to all apps subscribed to the page.
    -- For apps other than the original CTA sender, the postback event will be delivered via the standby channel.
    , rpPayload :: Text
    -- ^ Payload parameter that was defined with the button.
    -- This is only visible to the app that sent the original template message.
    , rpReferral :: Maybe Referral
    -- ^ This section is present only if:
    --
    -- * The user entered the thread via an @m.me@ link with a ref parameter and tapped the Get Started button.
    -- * The user entered the thread by scanning a parametric Messenger Code and tapped the Get Started button.
    -- * This is the first postback after user came from a Messenger Conversation Ad.
    -- * The user entered the thread via Discover tab and tapped the Get Started button.
    --
    -- https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_referrals
    } deriving (Eq, Show)

-- | This is a postback of which the receiving app has not sent the template this postback is generated from.
data SecondaryPostback = SecondaryPostback
    { spTitle :: Text
    -- ^ Title for the CTA that was clicked on. This is sent to all apps subscribed to the page.
    -- For apps other than the original CTA sender, the postback event will be delivered via the standby channel.
    , spReferral :: Maybe Referral
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
  toJSON (PBRegular rp) = toJSON rp
  toJSON (PBSecondary sp) = toJSON sp

instance FromJSON Postback where
  parseJSON = withObject "Postback" $ \o -> do
      mPayload <- o .:? "payload"
      maybe (secondary o)
            (regular o)
            (mPayload :: Maybe Text)
    where regular o _ = PBRegular <$> parseJSON (Object o)
          secondary o = PBSecondary <$> parseJSON (Object o)

instance ToJSON RegularPostback where
  toJSON (RegularPostback title payload ref) =
      object' [ "title" .=! title
              , "payload" .=! payload
              , "referral" .=!! ref
              ]

instance FromJSON RegularPostback where
  parseJSON = withObject "RegularPostback" $ \o ->
        RegularPostback <$> o .: "title"
                        <*> o .: "payload"
                        <*> o .:? "referral"

instance ToJSON SecondaryPostback where
  toJSON (SecondaryPostback title ref) =
      object' [ "title" .=! title
              , "referral" .=!! ref
              ]

instance FromJSON SecondaryPostback where
  parseJSON = withObject "SecondaryPostback" $ \o ->
        SecondaryPostback <$> o .: "title"
                          <*> o .:? "referral"
