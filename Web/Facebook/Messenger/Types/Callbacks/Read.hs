{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.Read
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

TODO: Explanation and link to FB Docs
-}
module Web.Facebook.Messenger.Types.Callbacks.Read
  ( ReadCallback (..) )
where


import Data.Aeson

import Web.Facebook.Messenger.Types.Static


-- --------------- --
--  READ CALLBACK  --
-- --------------- --

data ReadCallback = ReadCallback
    { rWatermark :: Integer -- All messages that were sent before this timestamp were read
    , rSeq :: Maybe Integer  -- Sequence number
    } deriving (Eq, Show)


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
