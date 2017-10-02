{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.AppRoles
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

TODO: Explanation and link to FB Docs
-}
module Web.Facebook.Messenger.Types.Callbacks.AppRoles (
  AppRoles (..)
  )
where


import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text

import Web.Facebook.Messenger.Types.Static (AppRole)

-- | This callback is sent when a page admin changes the role of your application.
newtype AppRoles = AppRoles (HM.HashMap Text [AppRole])
  deriving (Eq, Show)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON AppRoles where
  toJSON (AppRoles o) = Object (toJSON <$> o)

instance FromJSON AppRoles where
  parseJSON = withObject "AppRoles" $ \o -> AppRoles <$> parseJSON (Object o)
