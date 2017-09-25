module Web.Facebook.Messenger.Types.Callbacks.Optin where


import Data.Text
import Data.Aeson

import Web.Facebook.Messenger.Types.Static

-- ---------------- --
--  OPTIN CALLBACK  --
-- ---------------- --

data Optin = Optin
  { oRef :: Text
  , oUserRef :: Maybe Text
  } deriving (Eq, Show)


-- ----------------- --
--  OPTIN INSTANCES  --
-- ----------------- --

instance ToJSON Optin where
  toJSON (Optin ref user_ref) =
      object' [ "ref" .=! ref
              , "user_ref" .=!! user_ref ]

instance FromJSON Optin where
  parseJSON = withObject "Optin" $ \o ->
      Optin <$> o .: "ref"
            <*> o .:? "user_ref"
