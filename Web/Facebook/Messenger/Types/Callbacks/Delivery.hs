{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.Delivery
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will occur when a message a page has sent has been delivered.
You can subscribe to this callback by selecting the @"message_deliveries"@ field when setting up your webhook.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/message-deliveries
-}
module Web.Facebook.Messenger.Types.Callbacks.Delivery (
  -- * Delivery Callback
  Delivery (..)
  )
where


import Data.Aeson
import Data.Text

import Web.Facebook.Messenger.Types.Static


-- ------------------- --
--  DELIVERY CALLBACK  --
-- ------------------- --

-- | This callback is sent if messages have arrived at a user.
-- This doesn't mean the user has read them. That's what "Read" is for.
--
-- Both `dMids` and `dWatermark` fields are used to determine which messages were delivered.
-- `dWatermark` is always present and `dMids` is sometimes present.
-- `dMids` provides delivery receipts on a per-message basis but may not be present
-- (due to backward compatibility reasons with older Messenger clients).
-- `dWatermark` is always present and is a timestamp indicating that all messages with a timestamp before `dWatermark` were delivered.
data Delivery = Delivery
    { dWatermark :: Integer -- ^ All messages that were sent before this timestamp were delivered
    , dMids :: [Text] -- ^ Array containing message IDs of messages that were delivered. Field may not be present.
    , dSeq :: Maybe Integer -- ^ Sequence number
    } deriving (Eq, Show)


-- -------------------- --
--  DELIVERY INSTANCES  --
-- -------------------- --

instance FromJSON Delivery where
  parseJSON = withObject "Delivery" $ \o ->
      Delivery <$> o .: "watermark"
               <*> o .:? "mids" .!= []
               <*> o .:? "seq"


instance ToJSON Delivery where
  toJSON (Delivery watermark mids seq') =
      object' [ "watermark" .=! watermark
              , mEmptyList "mids" mids
              , "seq" .=!! seq'
              ]
