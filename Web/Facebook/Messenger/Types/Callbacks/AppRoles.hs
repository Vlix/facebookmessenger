module Web.Facebook.Messenger.Types.Callbacks.AppRoles where


import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text

import Web.Facebook.Messenger.Types.Static (AppRole)

-- | This callback is sent when a page admin changes the role of your application.
data AppRoles = AppRoles (HM.HashMap Text [AppRole])
  deriving (Eq, Show)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON AppRoles where
  toJSON (AppRoles o) = Object (toJSON <$> o)

instance FromJSON AppRoles where
  parseJSON = withObject "AppRoles" $ \o -> AppRoles <$> parseJSON (Object o)
