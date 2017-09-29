module Web.Facebook.Messenger.Types.Callbacks.TakeThreadControl
  ( TakeThread (..)
  )
where


import Data.Aeson
import Data.Text (Text)


-- | This callback is sent when thread ownership for a user has been taken away from your application. 
data TakeThread = TakeThread
    { ttPreviousOwnderAppId :: Text
    , ttMetaData :: Text
    } deriving (Eq, Show)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON TakeThread where
  toJSON (TakeThread appid metadata) =
      object [ "previous_owner_app_id" .= appid
             , "metadata" .= metadata
             ]

instance FromJSON TakeThread where
  parseJSON = withObject "TakeThread" $ \o ->
      TakeThread <$> o .: "previous_owner_app_id"
                 <*> o .: "metadata"
