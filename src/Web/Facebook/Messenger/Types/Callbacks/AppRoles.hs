{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.AppRoles
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will occur when a page admin changes the role of your application.
An app can be assigned the roles of `PrimaryReceiver` or `SecondaryReceiver`.

You can subscribe to this callback by selecting the @"messaging_handovers"@ field when setting up your webhook.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/app_role
-}
module Web.Facebook.Messenger.Types.Callbacks.AppRoles (
  -- * App Roles Callback
  AppRoles (..)
  )
where


import Data.Aeson
import qualified Data.Aeson.KeyMap as KM

import Web.Facebook.Messenger.Types.Static (AppRole)

-- | This callback is sent when a page admin changes the role of your application.
--
-- (My suspicion is that the `Text` key is an `AppId`, but untested)
newtype AppRoles = AppRoles (KM.KeyMap [AppRole])
  deriving stock (Eq, Show, Read)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON AppRoles where
  toJSON (AppRoles o) = Object (toJSON <$> o)

instance FromJSON AppRoles where
  parseJSON = withObject "AppRoles" $ \o -> AppRoles <$> parseJSON (Object o)
