{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.Read
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will occur when a message a page has sent has been read by the user.
You can subscribe to this callback by selecting the @"message_reads"@ field when setting up your webhook.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/message-reads
-}
module Web.Facebook.Messenger.Types.Callbacks.Read (
  -- * Message Read Callback
  ReadCallback (..)
  )
where


import Data.Aeson

import Web.Facebook.Messenger.Internal


-- --------------- --
--  READ CALLBACK  --
-- --------------- --

-- | Callback that certain messages have been read
--
-- The `rWatermark` field is used to determine which messages were read.
-- It represents a timestamp indicating that all messages with a timestamp before watermark were read by the recipient.
data ReadCallback = ReadCallback
    { rWatermark :: Integer -- ^ All messages that were sent before this timestamp were read
    , rSeq :: Maybe Integer  -- ^ Sequence number
    } deriving stock (Eq, Show, Read, Ord)


-- ---------------- --
--  READ INSTANCES  --
-- ---------------- --

instance FromJSON ReadCallback where
  parseJSON = withObject "ReadCallback" $ \o ->
      ReadCallback <$> o .: "watermark"
                   <*> o .:? "seq"

instance ToJSON ReadCallback where
  toJSON (ReadCallback watermark seq') =
      object' [ "watermark" .=! watermark
              , "seq" .=!! seq'
              ]
