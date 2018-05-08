{-|
Module      : Web.Facebook.Messenger.Types.Optin
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will occur:
* when the @"Send to Messenger"@ plugin has been tapped;
* when a user has accepted a message request using Customer Matching;
* or when user passed through a @Checkbox@ plugin

When using the plugin, the `oUserRef` parameter is set by the data-ref field on the @"Send to Messenger"@ plugin.
This field can be used by the developer to associate a click event on the plugin with a callback.

You can subscribe to this callback by selecting the @"messaging_optins"@ field when setting up your webhook.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_optins
-}
module Web.Facebook.Messenger.Types.Callbacks.Optin (
  -- * Plugin Opt-in Callback
  Optin (..)
  )
where


import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Static

-- ---------------- --
--  OPTIN CALLBACK  --
-- ---------------- --

-- | Plugin Opt-in callback sent through a Facebook webhook in case a user:
--
-- * clicks on a @"Send to Messenger"@ plugin;
-- * clicks on a @Checkbox@ plugin;
--
-- __N.B. This callback does not have a @CallbackSender@ in case the opt-in is via the @Checkbox@ plugin.__
-- __Use the given `oUserRef` instead for the first message sent to the user.__
data Optin = Optin
    { oRef :: Text -- ^ data-ref parameter that was defined with the entry point
    , oUserRef :: Maybe Text
    -- ^ In case the user went through the @Checkbox@ plugin,
    -- this is the @"user_ref"@ which you'll have to use as the `RecipientRef` in the
    -- first message you send to this user.
    } deriving (Eq, Show, Read, Ord)


-- ----------------- --
--  OPTIN INSTANCES  --
-- ----------------- --

instance ToJSON Optin where
  toJSON (Optin ref user_ref) =
      object' [ "ref" .=! ref
              , "user_ref" .=!! user_ref
              ]

instance FromJSON Optin where
  parseJSON = withObject "Optin" $ \o -> do
      Optin <$> o .: "ref"
            <*> o .:? "user_ref"
