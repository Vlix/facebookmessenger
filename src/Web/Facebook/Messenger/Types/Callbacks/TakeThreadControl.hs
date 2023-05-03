{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.TakeThreadControl
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will occur when thread ownership for a user has been taken away from your application.
`ttMetaData` contains free form string that the new thread owner has passed to your application.

You can subscribe to this callback by selecting the @"messaging_handovers"@ field when setting up your webhook.

https://developers.facebook.com/docs/messenger-platform/handover-protocol/take-thread-control
-}
module Web.Facebook.Messenger.Types.Callbacks.TakeThreadControl (
  -- * Take Thread Control
  TakeThread (..)
  )
where


import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests (AppId)


-- | A Secondary Receiver app will receive a @"take_thread_control"@ webhook event when it loses thread control.
data TakeThread = TakeThread
    { ttPreviousOwnderAppId :: AppId
    -- ^ App ID of the app taking control of a user's thread.
    -- (User from the top `RecipientSender`)
    , ttMetaData :: Maybe Text -- ^ Potential free form data sent from the app taking control of the thread.
    } deriving stock (Eq, Show, Read, Ord)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON TakeThread where
  toJSON (TakeThread appid metadata) =
      object' [ "previous_owner_app_id" .=! appid
              , "metadata" .=!! metadata
              ]

instance FromJSON TakeThread where
  parseJSON = withObject "TakeThread" $ \o ->
      TakeThread <$> o .: "previous_owner_app_id"
                 <*> o .:? "metadata"
