module Web.Facebook.Messenger.Types.Callbacks.Referral
  ( Referral (..)
  , RefShortLink (..)
  , RefAds (..)
  , RefMessengerCode (..)
  )
where


import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Static


-- ------------------- --
--  REFERRAL CALLBACK  --
-- ------------------- --

data Referral =
    ReferralLink RefShortLink
  | ReferralAds RefAds
  | ReferralCode RefMessengerCode
  | ReferralDiscover
  deriving (Eq, Show)

newtype RefShortLink = RefShortLink { rslRef :: Text }
  deriving (Eq, Show)

data RefAds = RefAds { raRef :: Maybe Text
                     , raAdId :: Text }
  deriving (Eq, Show)

newtype RefMessengerCode = RefMessengerCode { rmcRef :: Text }
  deriving (Eq, Show)


-- -------------------- --
--  REFERRAL INSTANCES  --
-- -------------------- --

instance FromJSON Referral where
  parseJSON = checkValue
      "Referral"
      "type"
      ("OPEN_THREAD" :: Text)
      $ \o -> do
         source <- o .: "source"
         case source of
           DISCOVER_TAB -> pure ReferralDiscover
           MESSENGER_CODE -> ReferralCode <$> (RefMessengerCode <$> o .: "ref")
           SHORTLINK -> ReferralLink <$> (RefShortLink <$> o .: "ref")
           ADS -> ReferralAds <$> (RefAds <$> o .:? "ref" <*> o .: "ad_id")

instance ToJSON Referral where
  toJSON referral = object' $ typ : more
    where typ = "type" .=! String "OPEN_THREAD"
          more = case referral of
                    ReferralLink (RefShortLink ref) ->
                        [ "source" .=! toJSON SHORTLINK
                        , "ref" .=! ref ]
                    ReferralAds (RefAds mRef ident) ->
                        [ "source" .=! toJSON ADS
                        , "ref" .=!! mRef
                        , "ad_id" .=! ident ]
                    ReferralCode (RefMessengerCode ref) ->
                        [ "source" .=! toJSON MESSENGER_CODE
                        , "ref" .=! ref ]
                    ReferralDiscover -> ["source" .=! toJSON DISCOVER_TAB]
