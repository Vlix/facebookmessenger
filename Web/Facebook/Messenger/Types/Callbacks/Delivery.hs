module Web.Facebook.Messenger.Types.Callbacks.Delivery where


import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- ------------------- --
--  DELIVERY CALLBACK  --
-- ------------------- --

data Delivery = Delivery
    { delivery_mids      :: Maybe [Text] -- Array containing message IDs of messages that were delivered. Field may not be present.
    , delivery_watermark :: Int -- All messages that were sent before this timestamp were delivered
    , delivery_seq       :: Int -- Sequence number
    }
  deriving (Eq, Show)


-- -------------------- --
--  DELIVERY INSTANCES  --
-- -------------------- --

instance FromJSON Delivery where
    parseJSON (Object o) = Delivery <$> o .:? "mids"
                                    <*> o .: "watermark"
                                    <*> o .: "seq"
    parseJSON wat = typeMismatch "Delivery" wat


instance ToJSON Delivery where
    toJSON (Delivery mids watermark seeq) = object [ "mids" .= mids
                                                   , "watermark" .= watermark
                                                   , "seq" .= seeq
                                                   ]
