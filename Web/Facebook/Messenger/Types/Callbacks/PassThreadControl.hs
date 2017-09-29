module Web.Facebook.Messenger.Types.Callbacks.PassThreadControl
  ( PassThread (..) )
where


import Data.Aeson
import Data.Text (Text)

-- | This callback is sent when thread ownership for a user has been passed to your application.
data PassThread = PassThread
    { ptNewOwnderAppId :: Text
    , ptMetaData :: Text
    } deriving (Eq, Show)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON PassThread where
  toJSON (PassThread appid metadata) =
      object [ "new_owner_app_id" .= appid
             , "metadata" .= metadata
             ]

instance FromJSON PassThread where
  parseJSON = withObject "PassThread" $ \o ->
      PassThread <$> o .: "new_owner_app_id"
                 <*> o .: "metadata"
