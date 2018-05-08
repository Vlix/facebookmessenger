{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.Referral
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will occur when the user already has a thread with the bot and user comes to the thread from:

* Following an @m.me@ link with a referral parameter
* Clicking on a Messenger Conversation Ad
* Scanning a parametric Messenger Code.
* The Discover Tab

To start receiving these events you need to subscribe to @"messaging_referrals"@ in the webhook settings for your app.
(For tracking referrals in new threads, refer to `Postback` Event.)

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_referrals
-}
module Web.Facebook.Messenger.Types.Callbacks.Referral (
  -- * Referral Callback
  Referral (..)
  , RefShortLink (..)
  , RefAds (..)
  , RefMessengerCode (..)
  , RefChatPlugin (..)
  )
where


import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Static


-- ------------------- --
--  REFERRAL CALLBACK  --
-- ------------------- --

-- | Types of referrals that might be sent through the webhook.
--
-- (only for users who are already in your bot)
data Referral =
    ReferralLink RefShortLink -- ^ User followed an @m.me@ link with a referral parameter
  | ReferralAds RefAds -- ^ User clicked on a Messenger Conversation Ad
  | ReferralCode RefMessengerCode -- ^ User scanned a parametric Messenger Code
  | ReferralChat RefChatPlugin -- ^ User started a chat with the Customer Chat Plugin
  | ReferralDiscover -- ^ User found your bot in the Discover Tab
  deriving (Eq, Show, Read, Ord)

-- | Referral parameter added to an @m.me@ link.
newtype RefShortLink = RefShortLink { rslRef :: Text }
  deriving (Eq, Show, Read, Ord)

-- | Referral parameter added to a Messenger Conversation Ad and the ID of that ad.
data RefAds = RefAds
  { raRef :: Maybe Text
  , raAdId :: Text
  } deriving (Eq, Show, Read, Ord)

-- | Referral parameter added to a Messenger Code.
newtype RefMessengerCode = RefMessengerCode { rmcRef :: Text }
  deriving (Eq, Show, Read, Ord)

-- | Referral parameters added when a user speaks
-- through a Customer Chat Plugin on a website
data RefChatPlugin = RefChatPlugin
  { rcpRef :: Maybe Text -- ^ Ref code from the plugin settings on the website
  , rcpRefUri :: URL -- ^ On which page the user initiated a chat
  } deriving (Eq, Show, Read, Ord)


-- -------------------- --
--  REFERRAL INSTANCES  --
-- -------------------- --

instance FromJSON Referral where
  parseJSON = checkValue
      "Referral"
      "type"
      ("OPEN_THREAD" :: Text)
      $ \o -> do
         source <- o .: "source"
         case source of
           DISCOVER_TAB -> pure ReferralDiscover
           MESSENGER_CODE -> ReferralCode <$> (RefMessengerCode <$> o .: "ref")
           SHORTLINK -> ReferralLink <$> (RefShortLink <$> o .: "ref")
           ADS -> ReferralAds <$> (RefAds <$> o .:? "ref" <*> o .: "ad_id")
           CUSTOMER_CHAT_PLUGIN -> ReferralChat <$> (RefChatPlugin <$> o .:? "ref" <*> o .: "referer_uri")

instance ToJSON Referral where
  toJSON referral = object' $ typ : more
    where typ = "type" .=! String "OPEN_THREAD"
          more = case referral of
                    ReferralLink (RefShortLink ref) ->
                        [ "source" .=! toJSON SHORTLINK
                        , "ref" .=! ref ]
                    ReferralAds (RefAds mRef ident) ->
                        [ "source" .=! toJSON ADS
                        , "ref" .=!! mRef
                        , "ad_id" .=! ident ]
                    ReferralCode (RefMessengerCode ref) ->
                        [ "source" .=! toJSON MESSENGER_CODE
                        , "ref" .=! ref ]
                    ReferralDiscover -> ["source" .=! toJSON DISCOVER_TAB]
                    ReferralChat (RefChatPlugin mRef uri) ->
                        [ "source" .=! toJSON CUSTOMER_CHAT_PLUGIN
                        , "ref" .=!! mRef
                        , "referer_uri" .=! uri
                        ]
