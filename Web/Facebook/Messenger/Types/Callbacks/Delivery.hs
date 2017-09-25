module Web.Facebook.Messenger.Types.Callbacks.Delivery where


import Data.Aeson
import Data.Text

import Web.Facebook.Messenger.Types.Static


-- ------------------- --
--  DELIVERY CALLBACK  --
-- ------------------- --

data Delivery = Delivery
  { dWatermark :: Integer -- All messages that were sent before this timestamp were delivered
  , dMids :: [Text] -- Array containing message IDs of messages that were delivered. Field may not be present.
  , dSeq :: Maybe Integer -- Sequence number
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
            , "mids"      .=! mids
            , "seq"       .=!! seq'
            ]
