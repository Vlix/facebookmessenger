{-# LANGUAGE DerivingStrategies #-}
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
  , postbackTitle
  , postbackReferral
  , RegularPostback (..)
  , SecondaryPostback (..)
  )
where


import Data.Text (Text)
import Data.Aeson

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Callbacks.Referral (Referral)


-- ------------------- --
--  POSTBACK CALLBACK  --
-- ------------------- --

-- | Postbacks occur when a `PostbackButton`, `GetStartedButton`, or `PersistentMenuItem` is tapped.
-- The payload field passed is defined in the above places.
data Postback = PBRegular RegularPostback
              | PBSecondary SecondaryPostback
  deriving stock (Eq, Show, Read, Ord)

-- | Convenience function to get the 'Maybe Title' from a 'Postback'
postbackTitle :: Postback -> Maybe Text
postbackTitle (PBRegular x) = rpTitle x
postbackTitle (PBSecondary x) = spTitle x

-- | Convenience function to get the 'Maybe Referral' from a 'Postback'
postbackReferral :: Postback -> Maybe Referral
postbackReferral (PBRegular x) = rpReferral x
postbackReferral (PBSecondary x) = spReferral x

-- | This postback has a payload included, meaning it's from the app that receives this is the app that sent it.
data RegularPostback = RegularPostback
    { rpTitle :: Maybe Text
    -- ^ Title for the CTA that was clicked on. This is sent to all apps subscribed to the page.
    -- For apps other than the original CTA sender, the postback event will be delivered via the standby channel.
    --
    -- Is sometimes @null@ for certain referral postbacks (e.g. "Get started" button in Customer Chat Plugins)
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
    } deriving stock (Eq, Show, Read, Ord)

-- | This is a postback of which the receiving app has not sent the template this postback is generated from.
data SecondaryPostback = SecondaryPostback
    { spTitle :: Maybe Text
    -- ^ Title for the CTA that was clicked on. This is sent to all apps subscribed to the page.
    -- For apps other than the original CTA sender, the postback event will be delivered via the standby channel.
    --
    -- Is sometimes @null@ for certain referral postbacks (e.g. "Get started" button in Customer Chat Plugins)
    , spReferral :: Maybe Referral
    -- ^ This section is present only if:
    --
    -- * The user entered the thread via an @m.me@ link with a ref parameter and tapped the Get Started button.
    -- * The user entered the thread by scanning a parametric Messenger Code and tapped the Get Started button.
    -- * This is the first postback after user came from a Messenger Conversation Ad.
    -- * The user entered the thread via Discover tab and tapped the Get Started button.
    --
    -- https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_referrals
    } deriving stock (Eq, Show, Read, Ord)


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
  toJSON (RegularPostback mTitle payload ref) =
      object' [ "title" .=!! mTitle
              , "payload" .=! payload
              , "referral" .=!! ref
              ]

instance FromJSON RegularPostback where
  parseJSON = withObject "RegularPostback" $ \o ->
        RegularPostback <$> o .:? "title"
                        <*> o .: "payload"
                        <*> o .:? "referral"

instance ToJSON SecondaryPostback where
  toJSON (SecondaryPostback mTitle ref) =
      object' [ "title" .=!! mTitle
              , "referral" .=!! ref
              ]

instance FromJSON SecondaryPostback where
  parseJSON = withObject "SecondaryPostback" $ \o ->
        SecondaryPostback <$> o .:? "title"
                          <*> o .:? "referral"
