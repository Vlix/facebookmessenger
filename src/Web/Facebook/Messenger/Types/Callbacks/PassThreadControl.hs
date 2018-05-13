{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.PassThreadControl
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will occur when thread ownership for a user has been passed to your application.
metadata contains free form string that the previous thread owner is passing to your application.

You can subscribe to this callback by selecting the @"messaging_handovers"@ field when setting up your webhook.

https://developers.facebook.com/docs/messenger-platform/handover-protocol/pass-thread-control
-}
module Web.Facebook.Messenger.Types.Callbacks.PassThreadControl (
  PassThread (..)
  )
where


import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Requests (AppId)
import Web.Facebook.Messenger.Types.Static

-- | This callback will occur when thread ownership for a user has been passed to your application.
data PassThread = PassThread
    { ptNewOwnderAppId :: AppId -- ^ App ID of the app receiving control over this user's thread
    , ptMetaData :: Maybe Text -- ^ Optional free form data sent from the control passing app
    } deriving (Eq, Show, Read, Ord)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON PassThread where
  toJSON (PassThread appid metadata) =
      object' [ "new_owner_app_id" .=! appid
              , "metadata" .=!! metadata
              ]

instance FromJSON PassThread where
  parseJSON = withObject "PassThread" $ \o ->
      PassThread <$> o .: "new_owner_app_id"
                 <*> o .:? "metadata"
