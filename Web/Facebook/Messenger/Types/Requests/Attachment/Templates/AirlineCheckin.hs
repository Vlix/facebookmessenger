{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineCheckin
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

https://developers.facebook.com/docs/messenger-platform/send-api-reference/airline-checkin-template
-}
module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineCheckin (
  -- * Check-in Template
  AirlineCheckin (..)
  , AirlineCheckinFlightInfo (..)
  , AirlineCheckinFlightSchedule (..)
  )
where


import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text, unpack)

import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.Airline (AirlineAirport)
import Web.Facebook.Messenger.Types.Static


-- ------------------------------- --
--  AIRLINE BOARDING PASS REQUEST  --
-- ------------------------------- --

-- | Template for sending a check-in reminder message.
data AirlineCheckin = AirlineCheckin
    { acIntroMessage :: Text -- ^ Introduction message
    , acLocale :: Text
    -- ^ locale must be a two-letter ISO 639-1 language code and a ISO 3166-1 alpha-2 region code
    -- separated by an underscore character. Used to translate field labels (e.g. en_US).
    , acThemeColor :: Maybe Text -- ^ Background color of the attachment. Must be a RGB hexadecimal string (default #009ddc)
    , acPnrNumber :: Text -- ^ Passenger name record number (Booking Number)
    , acFlightInfo :: [AirlineCheckinFlightInfo] -- ^ Information about a flight
    , acCheckinUrl :: URL -- ^ URL for passengers to check-in
    } deriving (Eq, Show)

-- | Information about a flight
data AirlineCheckinFlightInfo = AirlineCheckinFlightInfo
  { acfFlightNumber :: Text -- ^ Flight number
  , acfDepartureAirport :: AirlineAirport -- ^ Departure airport
  , acfArrivalAirport :: AirlineAirport -- ^ Arrival airport
  , acfFlightSchedule :: AirlineCheckinFlightSchedule -- ^ Schedule for the flight
  } deriving (Eq, Show)

-- | All times must be in the ISO 8601-based format
--
-- @YYYY-MM-DDThh:mm@
--
-- (e.g. 2015-09-26T10:30)
data AirlineCheckinFlightSchedule = AirlineCheckinFlightSchedule
  { acfBoardingTime :: Maybe Text -- ^ Boarding time in departure airport timezone
  , acfDepartureTime :: Text -- ^ Departure time in departure airport timezone
  , acfArrivalTime :: Text -- ^ Arrival time in arrival airport timezone
  } deriving (Eq, Show)

-- --------------------------- --
--  AIRLINE CHECKIN INSTANCES  --
-- --------------------------- --

instance ToJSON AirlineCheckin where
  toJSON (AirlineCheckin intro loc theme pnr flightInfo url) =
      object' [ "template_type" .=! String "airline_checkin"
              , "intro_message" .=! intro
              , "locale" .=! loc
              , "theme_color" .=!! theme
              , "pnr_number" .=! pnr
              , "flight_info" .=! flightInfo
              , "checkin_url" .=! url
              ]

instance ToJSON AirlineCheckinFlightInfo where
  toJSON (AirlineCheckinFlightInfo number depart arrive schedule) =
      object [ "flight_number" .= number
             , "departure_airport" .= depart
             , "arrival_airport" .= arrive
             , "flight_schedule" .= schedule
             ]

instance ToJSON AirlineCheckinFlightSchedule where
  toJSON (AirlineCheckinFlightSchedule boarding departure arrival) =
      object' [ "boarding_time" .=!! boarding
              , "departure_time" .=! departure
              , "arrival_time" .=! arrival
              ]


instance FromJSON AirlineCheckin where
  parseJSON = withObject "AirlineCheckin" $ \o -> do
      typ <- o .: "template_type" :: Parser Text
      unless (typ == "airline_checkin") $
        fail $ "AirlineCheckin: wrong \"type\" value: " `mappend` unpack typ
      AirlineCheckin <$> o .: "intro_message"
                     <*> o .: "locale"
                     <*> o .:? "theme_color"
                     <*> o .: "pnr_number"
                     <*> o .: "flight_info"
                     <*> o .: "checkin_url"

instance FromJSON AirlineCheckinFlightInfo where
  parseJSON = withObject "AirlineCheckinFlightInfo" $ \o ->
      AirlineCheckinFlightInfo <$> o .: "flight_number"
                               <*> o .: "departure_airport"
                               <*> o .: "arrival_airport"
                               <*> o .: "flight_schedule"

instance FromJSON AirlineCheckinFlightSchedule where
  parseJSON = withObject "AirlineCheckinFlightSchedule" $ \o ->
      AirlineCheckinFlightSchedule <$> o .:? "boarding_time"
                                   <*> o .: "departure_time"
                                   <*> o .: "arrival_time"
