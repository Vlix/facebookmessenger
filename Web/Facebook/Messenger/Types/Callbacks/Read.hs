module Web.Facebook.Messenger.Types.Callbacks.Read where


import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- --------------- --
--  READ CALLBACK  --
-- --------------- --

data FBCallbackRead = FBCallbackRead
    { fbcb_read_watermark :: Int -- All messages that were sent before this timestamp were read
    , fbcb_read_seq       :: Int -- Sequence number
    }


-- ---------------- --
--  READ INSTANCES  --
-- ---------------- --

instance FromJSON FBCallbackRead where
    parseJSON (Object o) = FBCallbackRead <$> o .: "watermark"
                                          <*> o .: "seq"
    parseJSON wat = typeMismatch "FBCallbackRead" wat


instance ToJSON FBCallbackRead where
    toJSON (FBCallbackRead watermark seq') = object [ "watermark" .= watermark
                                                    , "seq" .= seq'
                                                    ] 
