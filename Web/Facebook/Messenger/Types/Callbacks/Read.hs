module Web.Facebook.Messenger.Types.Callbacks.Read where


import Data.Aeson

import Web.Facebook.Messenger.Types.Static

-- --------------- --
--  READ CALLBACK  --
-- --------------- --

data ReadCallback = ReadCallback
  { rWatermark :: Integer -- All messages that were sent before this timestamp were read
  , rSeq :: Maybe Integer } -- Sequence number
  deriving (Eq, Show)


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
            , "seq"       .=!! seq'
            ] 
