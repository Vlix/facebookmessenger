module Web.Facebook.Messenger.Types.Callbacks.Read where


import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- --------------- --
--  READ CALLBACK  --
-- --------------- --

data ReadCallback = ReadCallback
    { read_watermark :: Int -- All messages that were sent before this timestamp were read
    , read_seq       :: Maybe Int -- Sequence number
    }
  deriving (Eq, Show)


-- ---------------- --
--  READ INSTANCES  --
-- ---------------- --

instance FromJSON ReadCallback where
    parseJSON (Object o) = ReadCallback <$> o .: "watermark"
                                        <*> o .:? "seq"
    parseJSON wat = typeMismatch "ReadCallback" wat


instance ToJSON ReadCallback where
    toJSON (ReadCallback watermark seq') = object [ "watermark" .= watermark
                                                  , "seq" .= seq'
                                                  ] 
