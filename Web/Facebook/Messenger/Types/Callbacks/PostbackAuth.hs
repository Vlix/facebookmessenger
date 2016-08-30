module Web.Facebook.Messenger.Types.Callbacks.PostbackAuth where


import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- ------------------- --
--  POSTBACK CALLBACK  --
-- ------------------- --

newtype Postback =
    Postback { postback_payload :: Text } -- Payload parameter that was defined with the button
  deriving (Eq, Show)


-- --------------- --
--  AUTH CALLBACK  --
-- --------------- --

newtype Optin =
    Optin { optin_ref :: Text } -- `data-ref` parameter that was defined with the entry point
  deriving (Eq, Show)


-- -------------------- --
--  POSTBACK INSTANCES  --
-- -------------------- --

instance ToJSON Postback where
    toJSON (Postback payload) = object [ "payload" .= payload ]

instance FromJSON Postback where
    parseJSON (Object o) = Postback <$> o .: "payload"
    parseJSON wat = typeMismatch "Postback" wat


-- -------------------- --
--  AUTH INSTANCES  --
-- -------------------- --

instance ToJSON Optin where
    toJSON (Optin ref) = object [ "ref" .= ref ]

instance FromJSON Optin where
    parseJSON (Object o) = Optin <$> o .: "ref"
    parseJSON wat = typeMismatch "Optin" wat
