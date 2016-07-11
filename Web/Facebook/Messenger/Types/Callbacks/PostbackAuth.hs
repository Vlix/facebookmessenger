module Web.Facebook.Messenger.Types.Callbacks.PostbackAuth where


import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- ------------------- --
--  POSTBACK CALLBACK  --
-- ------------------- --

newtype FBCallbackPostback =
    FBCallbackPostback { fbcb_postback_payload :: Text } -- Payload parameter that was defined with the button


-- --------------- --
--  AUTH CALLBACK  --
-- --------------- --

newtype FBCallbackOptin =
    FBCallbackOptin { fbcb_optin_ref :: Text } -- `data-ref` parameter that was defined with the entry point


-- -------------------- --
--  POSTBACK INSTANCES  --
-- -------------------- --

instance ToJSON FBCallbackPostback where
    toJSON (FBCallbackPostback payload) = object [ "payload" .= payload ]

instance FromJSON FBCallbackPostback where
    parseJSON (Object o) = FBCallbackPostback <$> o .: "payload"
    parseJSON wat = typeMismatch "FBCallbackPostback" wat


-- -------------------- --
--  AUTH INSTANCES  --
-- -------------------- --

instance ToJSON FBCallbackOptin where
    toJSON (FBCallbackOptin ref) = object [ "ref" .= ref ]

instance FromJSON FBCallbackOptin where
    parseJSON (Object o) = FBCallbackOptin <$> o .: "ref"
    parseJSON wat = typeMismatch "FBCallbackOptin" wat
