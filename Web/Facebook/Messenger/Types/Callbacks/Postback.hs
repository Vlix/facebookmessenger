module Web.Facebook.Messenger.Types.Callbacks.Postback where


import Data.Text
import Data.Aeson

import Web.Facebook.Messenger.Types.Callbacks.Referral (Referral)
import Web.Facebook.Messenger.Types.Static


-- ------------------- --
--  POSTBACK CALLBACK  --
-- ------------------- --

data Postback = Postback { pbTitle :: Text -- Payload parameter that was defined with the button
                         , pbPayload :: Text
                         , pbReferral :: Maybe Referral }
  deriving (Eq, Show)


-- -------------------- --
--  POSTBACK INSTANCES  --
-- -------------------- --

instance ToJSON Postback where
  toJSON (Postback title payload ref) =
      object' [ "title" .=! title
              , "payload" .=! payload
              , "referral" .=!! ref ]

instance FromJSON Postback where
  parseJSON = withObject "Postback" $ \o ->
        Postback <$> o .: "title"
                 <*> o .: "payload"
                 <*> o .:? "referral"
