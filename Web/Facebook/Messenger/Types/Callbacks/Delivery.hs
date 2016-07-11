module Web.Facebook.Messenger.Types.Callbacks.Delivery where


import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- ------------------- --
--  DELIVERY CALLBACK  --
-- ------------------- --

data FBCallbackDelivery = FBCallbackDelivery
    { fbcb_delivery_mids      :: Maybe [Text] -- Array containing message IDs of messages that were delivered. Field may not be present.
    , fbcb_delivery_watermark :: Int -- All messages that were sent before this timestamp were delivered
    , fbcb_delivery_seq       :: Int -- Sequence number
    }


-- -------------------- --
--  DELIVERY INSTANCES  --
-- -------------------- --

instance FromJSON FBCallbackDelivery where
    parseJSON (Object o) = FBCallbackDelivery <$> o .:? "mids"
                                              <*> o .: "watermark"
                                              <*> o .: "seq"
    parseJSON wat = typeMismatch "FBCallbackDelivery" wat


instance ToJSON FBCallbackDelivery where
    toJSON (FBCallbackDelivery mids watermark seeq) = object [ "mids" .= mids
                                                             , "watermark" .= watermark
                                                             , "seq" .= seeq
                                                             ]
