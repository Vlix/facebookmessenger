{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineFlightUpdate
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Template for sending a flight update to a user.

https://developers.facebook.com/docs/messenger-platform/send-api-reference/airline-update-template
-}
module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineFlightUpdate (
  -- * Flight Update Template
  AirlineFlightUpdate (..)
  )
where


import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text, unpack)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.Airline (AirlineFlightInfo)
import Web.Facebook.Messenger.Types.Static


-- ------------------------------- --
--  AIRLINE BOARDING PASS REQUEST  --
-- ------------------------------- --

-- | Send flight status update message.
data AirlineFlightUpdate = AirlineFlightUpdate
    { afuIntroMessage :: Maybe Text -- ^ Introduction message
    , afuUpdateType :: AirlineUpdateType -- ^ Type of update for this notification
    , afuLocale :: Text
    -- ^ Locale must be a two-letter ISO 639-1 language code and a ISO 3166-1 alpha-2 region code
    -- separated by an underscore character. Used to translate field labels (e.g. en_US)
    , afuThemeColor :: Maybe Text -- ^ Background color of the attachment. Must be a RGB hexadecimal string (default #009ddc)
    , afuPnrNumber :: Text -- ^ Passenger name record number (Booking Number)
    , afuUpdateFlightInfo :: AirlineFlightInfo -- ^ Information about a flight
    } deriving stock (Eq, Show, Read, Ord)


-- --------------------------- --
--  AIRLINE CHECKIN INSTANCES  --
-- --------------------------- --

instance ToJSON AirlineFlightUpdate where
  toJSON afu =
      object' [ "template_type" .=! String "airline_update"
              , "intro_message" .=!! afuIntroMessage afu
              , "update_type" .=! afuUpdateType afu
              , "locale" .=! afuLocale afu
              , "theme_color" .=!! afuThemeColor afu
              , "pnr_number" .=! afuPnrNumber afu
              , "update_flight_info" .=! afuUpdateFlightInfo afu
              ]


instance FromJSON AirlineFlightUpdate where
  parseJSON = withObject "AirlineFlightUpdate" $ \o -> do
      typ <- o .: "template_type" :: Parser Text
      unless (typ == "airline_update") $
        fail $ "AirlineFlightUpdate: wrong \"type\" value: " `mappend` unpack typ
      AirlineFlightUpdate <$> o .:? "intro_message"
                          <*> o .: "update_type"
                          <*> o .: "locale"
                          <*> o .:? "theme_color"
                          <*> o .: "pnr_number"
                          <*> o .: "update_flight_info"
