{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment.Templates.Airline
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Types that are used in multiple different Airline modules.
-}
module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.Airline (
  -- * Airline Types
  AirlineAirport (..)
  , AirlineFlightInfo (..)
  , AirlineFlightSchedule (..)
  )
where


import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Internal


-- ------------------ --
--  AIRLINE REQUESTS  --
-- ------------------ --

-- | Information about a certain airport
data AirlineAirport = AirlineAirport
  { aaAirportCode :: Text -- ^ Airport code (e.g. SFO\/AMS\/TYO)
  , aaCity :: Text -- ^ City name
  , aaTerminal :: Maybe Text -- ^ Terminal number
  , aaGate :: Maybe Text -- ^ Gate number
  } deriving stock (Eq, Show, Read, Ord)

-- | Information about a certain flight
data AirlineFlightInfo = AirlineFlightInfo
  { afiFlightNumber :: Text -- ^ Flight number
  , afiDepartureAirport :: AirlineAirport -- ^ Departure airport
  , afiArrivalAirport :: AirlineAirport -- ^ Arrival airport
  , afiFlightSchedule :: AirlineFlightSchedule -- ^ Schedule for the flight
  } deriving stock (Eq, Show, Read, Ord)

-- | Must all be in the ISO 8601-based format
--
-- @YYYY-MM-DDThh:mm@
--
-- (e.g. 2015-09-26T10:30)
data AirlineFlightSchedule = AirlineFlightSchedule
  { afsBoardingTime :: Maybe Text -- ^ Boarding time in departure airport timezone
  , afsDepartureTime :: Text -- ^ Departure time in departure airport timezone
  , afsArrivalTime :: Maybe Text -- ^ Arrival time in arrival airport timezone
  } deriving stock (Eq, Show, Read, Ord)


-- ----------------------- --
--    AIRLINE INSTANCES    --
-- ----------------------- --

instance ToJSON AirlineAirport where
  toJSON (AirlineAirport code city terminal gate) =
      object' [ "airport_code" .=! code
              , "city" .=! city
              , "terminal" .=!! terminal
              , "gate" .=!! gate
              ]

instance ToJSON AirlineFlightInfo where
  toJSON (AirlineFlightInfo number depart arrive schedule) =
      object [ "flight_number" .= number
             , "departure_airport" .= depart
             , "arrival_airport" .= arrive
             , "flight_schedule" .= schedule
             ]

instance ToJSON AirlineFlightSchedule where
  toJSON (AirlineFlightSchedule boarding departure arrival) =
      object' [ "boarding_time" .=!! boarding
              , "departure_time" .=! departure
              , "arrival_time" .=!! arrival
              ]


-- ---------------------------- --
--  FromJSON AIRLINE INSTANCES  --
-- ---------------------------- --

instance FromJSON AirlineAirport where
  parseJSON = withObject "AirlineAirport" $ \o ->
      AirlineAirport <$> o .: "airport_code"
                     <*> o .: "city"
                     <*> o .:? "terminal"
                     <*> o .:? "gate"

instance FromJSON AirlineFlightInfo where
  parseJSON = withObject "AirlineFlightInfo" $ \o ->
      AirlineFlightInfo <$> o .: "flight_number"
                        <*> o .: "departure_airport"
                        <*> o .: "arrival_airport"
                        <*> o .: "flight_schedule"

instance FromJSON AirlineFlightSchedule where
  parseJSON = withObject "AirlineFlightSchedule" $ \o ->
      AirlineFlightSchedule <$> o .:? "boarding_time"
                            <*> o .: "departure_time"
                            <*> o .:? "arrival_time"
