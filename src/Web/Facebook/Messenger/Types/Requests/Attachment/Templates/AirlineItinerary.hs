{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineItinerary
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Template for sending a user his/her itinerary (/receipt)

https://developers.facebook.com/docs/messenger-platform/send-api-reference/airline-itinerary-template
-}
module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineItinerary (
  -- * Itinerary Template
  AirlineItinerary (..)
  , PassengerInfo (..)
  , ItineraryFlightInfo (..)
  , PassengerSegmentInfo (..)
  , AirlineProductInfo (..)
  , PriceInfo (..)
  )
where


import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text, unpack)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.Airline (AirlineAirport)
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineCheckin (AirlineCheckinFlightSchedule)


-- ------------------------------- --
--  AIRLINE BOARDING PASS REQUEST  --
-- ------------------------------- --

-- | Send a confirmation message that contains the itinerary and receipt.
data AirlineItinerary = AirlineItinerary
    { aiIntroMessage :: Text -- ^ Introduction message
    , aiLocale :: Text
    -- ^ locale must be a two-letter ISO 639-1 language code and a ISO 3166-1 alpha-2 region code
    -- separated by an underscore character. Used to translate field labels (e.g. en_US).
    , aiThemeColor :: Maybe Text -- ^ Background color of the attachment. Must be a RGB hexadecimal string (default #009ddc)
    , aiPnrNumber :: Text -- ^ Passenger name record number (Booking Number)
    , aiPassengerInfo :: [PassengerInfo] -- ^ Information about a passenger
    , aiFlightInfo :: [ItineraryFlightInfo] -- ^ Information about a flight
    , aiPassengerSegmentInfo :: [PassengerSegmentInfo] -- ^ Information unique to passenger/segment pair
    , aiPriceInfo :: [PriceInfo] -- ^ Itemization of the total price (limited to 4)
    , aiBasePrice :: Maybe Double -- ^ Base price amount
    , aiTax :: Maybe Double -- ^ Tax amount
    , aiTotalPrice :: Double -- ^ Total price for the booking
    , aiCurrency :: Text
    -- ^ Pricing currency (must be a three digit ISO-4217-3 code.)
    --
    -- https://developers.facebook.com/docs/payments/reference/supportedcurrencies
    } deriving stock (Eq, Show, Read, Ord)

-- | Information about a passenger
data PassengerInfo = PassengerInfo
  { apiPassengerId :: Text -- ^ Passenger ID (must be unique in itinerary)
  , apiTicketNumber :: Maybe Text -- ^ Ticket number
  , apiName :: Text -- ^ Full name of passenger, including title (e.q. "Farbound Smith Jr")
  } deriving stock (Eq, Show, Read, Ord)

-- | Information about a flight
data ItineraryFlightInfo = ItineraryFlightInfo
  { aifConnectionId :: Text -- ^ Used to group segments of a connection together (must be unique in itinerary)
  , aifSegmentId :: Text -- ^ Should be the same as `apsSegmentId` of `PassengerSegmentInfo`
  , aifFlightNumber :: Text -- ^ Flight number
  , aifAircraftType :: Maybe Text -- ^ Aircraft type (e.g. Boeing 787)
  , aifDepartureAirport :: AirlineAirport -- ^ Departure airport
  , aifArrivalAirport :: AirlineAirport -- ^ Arrival airport
  , aifFlightSchedule :: AirlineCheckinFlightSchedule -- ^ Schedule for the flight
  , aifTravelClass :: Text -- ^ Travel class (appears as Cabin)
  } deriving stock (Eq, Show, Read, Ord)

-- | Information unique to passenger/segment pair
data PassengerSegmentInfo = PassengerSegmentInfo
  { apsSegmentId :: Text -- ^ Used to identify a flight segment (must be unique in itinerary)
  , apsPassengerId :: Text -- ^ Should be the same as `apiPassengerId` of `PassengerInfo`
  , apsSeat :: Text -- ^ Seat number for the passenger
  , apsSeatType :: Text -- ^ Seat type for the passenger (e.g. Economy comfort)
  , apsProductInfo :: [AirlineProductInfo] -- ^ List of products the passenger purchased (limited to 4)
  } deriving stock (Eq, Show, Read, Ord)

-- | List of products the passenger purchased
data AirlineProductInfo = AirlineProductInfo
  { aprTitle :: Text -- ^ Label for the additional field
  , aprValue :: Text -- ^ Value for the additional field
  } deriving stock (Eq, Show, Read, Ord)

-- | Itemization of the total price
data PriceInfo = PriceInfo
  { apiTitle :: Text -- ^ Price info title
  , apiAmount :: Double -- ^ Price amount
  , apiCurrency :: Maybe Text
  -- ^ Pricing currency (must be a three digit ISO-4217-3 code)
  --
  -- https://developers.facebook.com/docs/payments/reference/supportedcurrencies
  } deriving stock (Eq, Show, Read, Ord)


-- --------------------------- --
--  AIRLINE CHECKIN INSTANCES  --
-- --------------------------- --

instance ToJSON AirlineItinerary where
  toJSON ait =
      object' [ "template_type" .=! String "airline_itinerary"
              , "intro_message" .=! aiIntroMessage ait
              , "locale" .=! aiLocale ait
              , "theme_color" .=!! aiThemeColor ait
              , "pnr_number" .=! aiPnrNumber ait
              , "passenger_info" .=! aiPassengerInfo ait
              , "flight_info" .=! aiFlightInfo ait
              , "passenger_segment_info" .=! aiPassengerSegmentInfo ait
              , mEmptyList "price_info" $ aiPriceInfo ait
              , "base_price" .=!! aiBasePrice ait
              , "tax" .=!! aiTax ait
              , "total_price" .=! aiTotalPrice ait
              , "currency" .=! aiCurrency ait
              ]

instance ToJSON PassengerInfo where
  toJSON (PassengerInfo ident ticket name) =
      object' [ "passenger_id" .=! ident
              , "ticket_number" .=!! ticket
              , "name" .=! name
              ]

instance ToJSON ItineraryFlightInfo where
  toJSON aif =
      object' [ "connection_id" .=! aifConnectionId aif
              , "segment_id" .=! aifSegmentId aif
              , "flight_number" .=! aifFlightNumber aif
              , "aircraft_type" .=!! aifAircraftType aif
              , "departure_airport" .=! aifDepartureAirport aif
              , "arrival_airport" .=! aifArrivalAirport aif
              , "flight_schedule" .=! aifFlightSchedule aif
              , "travel_class" .=! aifTravelClass aif
              ]

instance ToJSON PassengerSegmentInfo where
  toJSON aps =
      object' [ "segment_id" .=! apsSegmentId aps
              , "passenger_id" .=! apsPassengerId aps
              , "seat" .=! apsSeat aps
              , "seat_type" .=! apsSeatType aps
              , mEmptyList "product_info" $ take 4 $ apsProductInfo aps
              ]

instance ToJSON AirlineProductInfo where
  toJSON (AirlineProductInfo title value) =
      object [ "title" .= title
             , "value" .= value
             ]

instance ToJSON PriceInfo where
  toJSON (PriceInfo title amount currency) =
      object' [ "title" .=! title
              , "amount" .=! amount
              , "currency" .=!! currency
              ]


instance FromJSON AirlineItinerary where
  parseJSON = withObject "AirlineItinerary" $ \o -> do
      typ <- o .: "template_type" :: Parser Text
      unless (typ == "airline_itinerary") $
        fail $ "AirlineItinerary: wrong \"type\" value: " `mappend` unpack typ
      AirlineItinerary <$> o .: "intro_message"
                       <*> o .: "locale"
                       <*> o .:? "theme_color"
                       <*> o .: "pnr_number"
                       <*> o .: "passenger_info"
                       <*> o .: "flight_info"
                       <*> o .: "passenger_segment_info"
                       <*> o .:? "price_info" .!= []
                       <*> o .:? "base_price"
                       <*> o .:? "tax"
                       <*> o .: "total_price"
                       <*> o .: "currency"

instance FromJSON PassengerInfo where
  parseJSON = withObject "PassengerInfo" $ \o ->
      PassengerInfo <$> o .: "passenger_id"
                    <*> o .:? "ticket_number"
                    <*> o .: "name"

instance FromJSON ItineraryFlightInfo where
  parseJSON = withObject "ItineraryFlightInfo" $ \o ->
      ItineraryFlightInfo <$> o .: "connection_id"
                          <*> o .: "segment_id"
                          <*> o .: "flight_number"
                          <*> o .:? "aircraft_type"
                          <*> o .: "departure_airport"
                          <*> o .: "arrival_airport"
                          <*> o .: "flight_schedule"
                          <*> o .: "travel_class"

instance FromJSON PassengerSegmentInfo where
  parseJSON = withObject "PassengerSegmentInfo" $ \o ->
      PassengerSegmentInfo <$> o .: "segment_id"
                           <*> o .: "passenger_id"
                           <*> o .: "seat"
                           <*> o .: "seat_type"
                           <*> o .:? "product_info" .!= []

instance FromJSON AirlineProductInfo where
  parseJSON = withObject "AirlineProductInfo" $ \o ->
      AirlineProductInfo <$> o .: "title"
                         <*> o .: "value"

instance FromJSON PriceInfo where
  parseJSON = withObject "PriceInfo" $ \o ->
      PriceInfo <$> o .: "title"
                <*> o .: "amount"
                <*> o .:? "currency"
