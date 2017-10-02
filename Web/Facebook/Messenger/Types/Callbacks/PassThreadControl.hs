{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.PassThreadControl
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

TODO: Explanation and link to FB Docs
-}
module Web.Facebook.Messenger.Types.Callbacks.PassThreadControl (
  PassThread (..)
  )
where


import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Requests (AppId)

-- This callback is sent when thread ownership for a user has been passed to your application.
data PassThread = PassThread
    { ptNewOwnderAppId :: AppId -- ^ TODO: ...
    , ptMetaData :: Text -- ^ TODO: ...
    } deriving (Eq, Show)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON PassThread where
  toJSON (PassThread appid metadata) =
      object [ "new_owner_app_id" .= appid
             , "metadata" .= metadata
             ]

instance FromJSON PassThread where
  parseJSON = withObject "PassThread" $ \o ->
      PassThread <$> o .: "new_owner_app_id"
                 <*> o .: "metadata"
