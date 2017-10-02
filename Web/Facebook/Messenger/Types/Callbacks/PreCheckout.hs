{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.PreCheckout
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

TODO: Explanation and link to FB Docs
-}
module Web.Facebook.Messenger.Types.Callbacks.PreCheckout
  ( PreCheckout (..) )
where


import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Callbacks.Payment (RequestedUserInfo, Amount)


-- This callback is sent just before charging the user
-- You need to respond with { "success": true } to let the payment go through.
data PreCheckout = PreCheckout
    { cpPayload :: Text
    , cpRequestedUserInfo :: RequestedUserInfo
    , cpAmount :: Amount
    } deriving (Eq, Show)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON PreCheckout where
  toJSON (PreCheckout payload rui amount) =
      object [ "payload" .= payload
             , "requested_user_info" .= rui
             , "amount" .= amount
             ]

instance FromJSON PreCheckout where
  parseJSON = withObject "PreCheckout" $ \o ->
      PreCheckout <$> o .: "payload"
                  <*> o .: "requested_user_info"
                  <*> o .: "amount"
