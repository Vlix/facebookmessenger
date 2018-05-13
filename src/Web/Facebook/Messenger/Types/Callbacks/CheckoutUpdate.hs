{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.CheckoutUpdate
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback enables you to update pricing for flexible-amount transactions on the checkout dialog displayed by the `BuyButton`.
After the `BuyButton` is tapped, a call is made to the webhook containing the person's shipping address.
This enables you to update pricing with shipping and taxes based on a person's location.
This callback is made each time the shipping address is changed.

You can subscribe to this callback by selecting the @"messaging_checkout_updates"@ field when setting up your webhook.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_checkout_updates
-}
module Web.Facebook.Messenger.Types.Callbacks.CheckoutUpdate (
  -- * Checkout Update Callback
  CheckoutUpdate (..)
  )
where


import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.Text

import Web.Facebook.Messenger.Types.Requests.Extra (TemplateAddress)


-- | This callback is used for flexible-amount transactions
--
-- You need to respond with a 200 OK and `CheckoutUpdateResponse` in the body of the response.
data CheckoutUpdate = CheckoutUpdate
    { cuPayload :: Text -- ^ Metadata defined in the Buy Button.
    , cuShipid  :: Text
    -- ^ ID of shipping address
    -- (Again, the documentation example is a Number,
    -- and the text says the shipping_address_id's a String...)
    , cuAddress :: TemplateAddress -- ^ Address of the user
    } deriving (Eq, Show, Read, Ord)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON CheckoutUpdate where
  toJSON (CheckoutUpdate payload shipid address) =
      object [ "payload" .= payload
             , "shipping_address" .= addIDtoAddress
             ]
    where addIDtoAddress = changeObject addressValue $ HM.insert "id" shipIDValue
          shipIDValue    = toJSON shipid
          addressValue   = toJSON address
          changeObject (Object o) f = Object $ f o
          changeObject x          _ = x

instance FromJSON CheckoutUpdate where
  parseJSON = withObject "CheckoutUpdate" $ \o -> do
      shipAddr <- o .: "shipping_address"
      case shipAddr of
        Object ob -> do
            ident <- ob .: "id"
            shipId <- case ident of
                        i@(Number _) -> pack . show <$> (parseJSON i :: Parser Integer)
                        (String t) -> pure t
                        _ -> fail "CheckoutUpdate: invalid type in \"id\" field."
            CheckoutUpdate <$> o .: "payload"
                           <*> pure shipId
                           <*> o .: "shipping_address"
        _ -> fail "CheckoutUpdate: \"shipping_address\" value not an object."
