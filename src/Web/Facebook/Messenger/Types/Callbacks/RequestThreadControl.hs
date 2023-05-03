{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.RequestThreadControl
Copyright   : (c) Felix Paulusma, 2016-2018
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will be sent to the Primary Receiver app
when a Secondary Receiver app calls the Request Thread
Control API. The Primary Receiver may then choose to
honor the request and pass thread control, or ignore
the request.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_handovers#request_thread_control
-}
module Web.Facebook.Messenger.Types.Callbacks.RequestThreadControl (
  -- * Request Thread Control
  RequestThread (..)
  )
where


import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests (AppId)


-- | A Secondary Receiver app will receive a @"take_thread_control"@ webhook event when it loses thread control.
data RequestThread = RequestThread
    { rtPreviousOwnderAppId :: AppId
    -- ^ App ID of the Secondary Receiver that is requesting thread control.
    -- (User from the top `RecipientSender`)
    , rtMetaData :: Maybe Text
    -- ^ Custom string specified in the API request.
    } deriving stock (Eq, Show, Read, Ord)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON RequestThread where
  toJSON (RequestThread appid metadata) =
      object' [ "requested_owner_app_id" .=! appid
              , "metadata" .=!! metadata
              ]

instance FromJSON RequestThread where
  parseJSON = withObject "RequestThread" $ \o ->
      RequestThread <$> o .: "requested_owner_app_id"
                 <*> o .:? "metadata"
