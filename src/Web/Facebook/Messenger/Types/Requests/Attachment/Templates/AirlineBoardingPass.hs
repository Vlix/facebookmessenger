{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineBoardingPass
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Send a message that contains boarding passes for one or more flights or one more passengers.
Message bubbles will be grouped by flight information. (/if the flight information matches, all passengers will be share the same bubble/)

Multiple bubbles are automatically sent for all `BoardingPass` elements with different values for `AirlineFlightInfo`.
In the future, we may group all boarding passes into the same bubble.

https://developers.facebook.com/docs/messenger-platform/send-messages/template/airline-boarding-pass
-}
module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineBoardingPass (
  -- * Boarding Pass Template
  AirlineBoardingPass (..)
  , BoardingPass (..)
  -- ** QR / Barcode
  , qrCode
  , barCode
  , AirlineQRBarCode (..)
  , AirlineQRCode (..)
  , AirlineBarCode (..)
  -- * Custom Fields
  , AirlineField (..)
  )
where


import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, unpack)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.Airline (AirlineFlightInfo)
import Web.Facebook.Messenger.Types.Static (URL)


-- ------------------------------- --
--  AIRLINE BOARDING PASS REQUEST  --
-- ------------------------------- --

-- | Template for sending boarding passes to users
data AirlineBoardingPass = AirlineBoardingPass
    { abptIntroMessage :: Text -- ^ Introduction message
    , abptLocale :: Text
    -- ^ Locale must be a two-letter ISO 639-1 language code and a ISO 3166-1 alpha-2 region code
    -- separated by an underscore character. Used to translate field labels (e.g. en_US).
    --
    -- https://developers.facebook.com/docs/messenger-platform/messenger-profile/supported-locales
    , abptThemeColor :: Maybe Text -- ^ Background color of the attachment. Must be a RGB hexadecimal string (default #009ddc)
    , abptBoardingPass :: [BoardingPass] -- ^ Boarding passes for passengers
    } deriving (Eq, Show, Read, Ord)

-- | Boarding pass for a passenger
--
-- N.B. `abpAuxiliaryFields` and `abpSecondaryFields` are rendered left first to right last
data BoardingPass = BoardingPass
    { abpPassengerName :: Text -- ^ Passenger name (e.g. @"SMITH/NICOLAS"@)
    , abpPnrNumber :: Text -- ^ Passenger name record number (Booking Number)
    , abpSeat :: Maybe Text -- ^ Seat number for passenger
    , abpAuxiliaryFields :: [AirlineField] -- ^ Flexible information to display in the auxiliary section (limited to 5)
    , abpSecondaryFields :: [AirlineField] -- ^ Flexible information to display in the secondary section (limited to 5)
    , abpLogoImageUrl :: URL -- ^ URL for the logo image
    , abpHeaderImageUrl :: Maybe URL -- ^ URL for the header image
    , abpHeaderTextField :: Maybe AirlineField -- ^ Text for the header field
    , abpQrBarCode :: AirlineQRBarCode -- ^ /use `qrCode` or `barCode` to construct the `AirlineQRBarCode`/
    , abpAboveBarCodeImageUrl :: URL -- ^ URL of thin image above the barcode
    , abpFlightInfo :: AirlineFlightInfo -- ^ Information about the flight
    } deriving (Eq, Show, Read, Ord)

-- | QR code or Bar code used as the boarding pass
data AirlineQRBarCode = AirlineQR AirlineQRCode
                      | AirlineBar AirlineBarCode
  deriving (Eq, Show, Read, Ord)

-- | Takes Aztec or QR text.
qrCode :: Text -> AirlineQRBarCode
qrCode = AirlineQR . AirlineQRCode

-- | Aztec or QR code
newtype AirlineQRCode =
          AirlineQRCode { getQRCode :: Text }
  deriving (Eq, Show, Read, Ord, FromJSON, ToJSON)

-- | Takes an image URL.
barCode :: URL -> AirlineQRBarCode
barCode = AirlineBar . AirlineBarCode

-- | URL of the barcode image
newtype AirlineBarCode =
          AirlineBarCode { barCodeImageUrl :: URL }
  deriving (Eq, Show, Read, Ord, FromJSON, ToJSON)

-- | Custom field to add information to the `BoardingPass`
data AirlineField = AirlineField
  { afLabel :: Text -- Label for the additional field
  , afValue :: Text -- Value for the additional field
  } deriving (Eq, Show, Read, Ord)



-- ----------------------- --
--    AIRLINE INSTANCES    --
-- ----------------------- --

instance ToJSON AirlineBoardingPass where
  toJSON (AirlineBoardingPass intro loc theme passes) =
      object' [ "template_type" .=! String "airline_boardingpass"
              , "intro_message" .=! intro
              , "locale" .=! loc
              , "theme_color" .=!! theme
              , "boarding_pass" .=! passes
              ]

instance ToJSON BoardingPass where
  toJSON abp = object' $ extra : basis
   where
    extra = case abpQrBarCode abp of
      AirlineQR qr -> "qr_code" .=! qr
      AirlineBar bar -> "barcode_image_url" .=! bar
    basis = [ "passenger_name" .=! abpPassengerName abp
            , "pnr_number" .=! abpPnrNumber abp
            , "seat" .=!! abpSeat abp
            , "logo_image_url" .=! abpLogoImageUrl abp
            , "header_image_url" .=!! abpHeaderImageUrl abp
            , "header_text_field" .=!! abpHeaderTextField abp
            , "above_bar_code_image_url" .=! abpAboveBarCodeImageUrl abp
            , "flight_info" .=! abpFlightInfo abp
            , mEmptyList "auxiliary_fields" $ take 5 $ abpAuxiliaryFields abp
            , mEmptyList "secondary_fields" $ take 5 $ abpSecondaryFields abp
            ]

instance ToJSON AirlineField where
  toJSON (AirlineField label value) =
      object [ "label" .= label
             , "value" .= value
             ]


-- ---------------------------- --
--  FromJSON AIRLINE INSTANCES  --
-- ---------------------------- --

instance FromJSON AirlineBoardingPass where
  parseJSON = withObject "AirlineBoardingPass" $ \o -> do
      typ <- o .: "template_type" :: Parser Text
      unless (typ == "airline_boardingpass") $
        fail $ "AirlineBoardingPass: wrong \"type\" value: " `mappend` unpack typ
      AirlineBoardingPass <$> o .: "intro_message"
                          <*> o .: "locale"
                          <*> o .:? "theme_color"
                          <*> o .:? "boarding_pass" .!= []

instance FromJSON BoardingPass where
  parseJSON = withObject "BoardingPass" $ \o -> do
      let mQRCode = "qr_code" `HM.lookup` o
          mBarCode = "barcode_image_url" `HM.lookup` o
          eitherQrBar = case (mQRCode,mBarCode) of
            (Just v,Nothing) -> AirlineQR <$> parseJSON v
            (Nothing,Just v) -> AirlineBar <$> parseJSON v
            _ -> fail "BoardingPass: needs either \"qr_code\" or \"barcode_image_url\""
      BoardingPass <$> o .: "passenger_name"
                   <*> o .: "pnr_number"
                   <*> o .:? "seat"
                   <*> o .:? "auxiliary_fields" .!= []
                   <*> o .:? "secondary_fields" .!= []
                   <*> o .: "logo_image_url"
                   <*> o .:? "header_image_url"
                   <*> o .:? "header_text_field"
                   <*> eitherQrBar
                   <*> o .: "above_bar_code_image_url"
                   <*> o .: "flight_info"

instance FromJSON AirlineField where
  parseJSON = withObject "AirlineField" $ \o ->
      AirlineField <$> o .: "label"
                   <*> o .: "value"
