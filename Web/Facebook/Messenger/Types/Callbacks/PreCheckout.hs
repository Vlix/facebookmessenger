{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.PreCheckout
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will occur when a user clicks on Pay in the payment dialog, but before the user's card is charged.
This allows you to do any processing on your end before charging user's card.
You could check inventory levels or for price changes before accepting the payment.

Subscribe to this callback by selecting the @"messaging_pre_checkouts"@ option when setting up your webhook.
If your app does not subscribe to this event, after the user clicks on Pay we will process the payment directly.

The event is only called for payments triggered via `BuyButton`, and not for those triggered via webview.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_pre_checkouts
-}
module Web.Facebook.Messenger.Types.Callbacks.PreCheckout (
  -- * Pre-Checkout Callback
  PreCheckout (..)
  )
where


import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Callbacks.Payment (RequestedUserInfo, Amount)

-- | This callback is sent just before charging the user
--
-- You must respond to the callback with an HTTP status of @200@
-- and the body of the response must contain a success field
-- to indicate whether the pre checkout processing went through.
-- If success returned is false, we will not charge the user and
-- fail the payment flow. Otherwise we will let the payment go through.
data PreCheckout = PreCheckout
    { cpPayload :: Text -- ^ Metadata defined in the `BuyButton`.
    , cpRequestedUserInfo :: RequestedUserInfo -- ^ Information that was requested from the user by the Buy Button.
    , cpAmount :: Amount -- ^ Total amount of transaction.
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
