module Web.Facebook.Messenger.Types.Callbacks.PostbackOptin where


import           Control.Applicative  ((<|>))
import           Data.Text
import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)
import qualified Data.HashMap.Strict  as HM

import           Web.Facebook.Messenger.Types.Static (ReferralSource)


-- ------------------- --
--  POSTBACK CALLBACK  --
-- ------------------- --

data Postback = Postback { postback_payload :: Text } -- Payload parameter that was defined with the button
              | RefPostback { postback_payload  :: Text
                            , postback_referral :: Referral }
  deriving (Eq, Show)

data Referral = Referral { referral_ref    :: Text
                         , referral_source :: ReferralSource } -- Can only be "SHORTLINK" for now
                      -- , referral_type   :: Text } -- Will always be "OPEN_THREAD" for m.me/PAGE_NAME?ref= links
  deriving (Eq, Show)

-- ---------------- --
--  OPTIN CALLBACK  --
-- ---------------- --

newtype Optin = Optin { optin_ref :: Text } -- `data-ref` parameter that was defined with the entry point
  deriving (Eq, Show)

data OptinRef = OptinRef
  { optin_refcb_ref      :: Text
  , optin_refcb_user_ref :: Text
  } deriving (Eq, Show)


-- -------------------- --
--  POSTBACK INSTANCES  --
-- -------------------- --

instance ToJSON Postback where
  toJSON (Postback payload) = object [ "payload" .= payload ]
  toJSON (RefPostback payload referral) =
    object [ "payload"  .= payload
           , "referral" .= referral
           ]

instance ToJSON Referral where
  toJSON (Referral ref source {-typ-} ) =
    object [ "ref"    .= ref
           , "source" .= source
           , "type"   .= String "OPEN_THREAD"
           ]

instance FromJSON Postback where
  parseJSON (Object o) = RefPostback <$> o .: "payload"
                                     <*> o .: "referral"
                     <|> Postback <$> o .: "payload"
  parseJSON wat = typeMismatch "Postback" wat

instance FromJSON Referral where
  parseJSON (Object o) = case HM.lookup "type" o of
      Just "OPEN_THREAD" -> Referral <$> o .: "ref"
                                     <*> o .: "source"
                                     -- <*> o .: "type"
      _ -> fail "Expected OPEN_THREAD as type value in Referral object"
  parseJSON wat = typeMismatch "Referral" wat

-- ----------------- --
--  OPTIN INSTANCES  --
-- ----------------- --

instance ToJSON Optin where
  toJSON (Optin ref) = object [ "ref" .= ref ]

instance FromJSON Optin where
  parseJSON (Object o) = Optin <$> o .: "ref"
  parseJSON wat = typeMismatch "Optin" wat


instance ToJSON OptinRef where
  toJSON (OptinRef ref user_ref) =
    object [ "ref" .= ref
           , "user_ref" .= user_ref
           ]

instance FromJSON OptinRef where
  parseJSON (Object o) = OptinRef <$> o .: "ref"
                                          <*> o .: "user_ref"
  parseJSON wat = typeMismatch "OptinRef" wat
