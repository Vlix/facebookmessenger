{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Facebook.Messenger.Types.Requests.Airline where


import           Data.Text            (Text)
import           Data.Aeson
import qualified Data.HashMap.Strict  as HM

import           Web.Facebook.Messenger.Types.Static


-- ------------------ --
--  AIRLINE REQUESTS  --
-- ------------------ --

data AirlinePassengerInfo = AirlinePassengerInfo
  { air_personalinfo_passenger_id  :: Text       -- must be unique in itinerary
  , air_personalinfo_ticket_number :: Maybe Text -- Ticket number
  , air_personalinfo_name          :: Text       -- Full name of passenger, including title
  } deriving (Eq, Show)

data AirlineItineraryFlightInfo = AirlineItineraryFlightInfo
  { air_iflightinfo_connection_id     :: Text       -- Used to group segments of a connection together (must be unique in itinerary)
  , air_iflightinfo_segment_id        :: Text       -- `segment_id` of `passenger_segment_info` object
  , air_iflightinfo_flight_number     :: Text       -- Flight number
  , air_iflightinfo_aircraft_type     :: Maybe Text -- Aircraft type (e.g. Boeing 787)
  , air_iflightinfo_departure_airport :: AirlineAirport -- Departure airport
  , air_iflightinfo_arrival_airport   :: AirlineAirport -- Arrival airport
  , air_iflightinfo_flight_schedule   :: AirlineFlightSchedule  -- Schedule for the flight
  , air_iflightinfo_travel_class      :: AirlineTravelClassType -- Travel class (ECONOMY, BUSINESS or FIRST_CLASS)
  } deriving (Eq, Show)

data AirlineAirport = AirlineAirport
  { air_airport_airport_code :: Text       -- Airport code (e.g. SFO/AMS/TYO)
  , air_airport_city         :: Text       -- City name
  , air_airport_terminal     :: Maybe Text -- Terminal number
  , air_airport_gate         :: Maybe Text -- Gate number
  } deriving (Eq, Show)

-- all times in schedule must be in the ISO 8601-based format YYYY-MM-DDThh:mm (e.g. 2015-09-26T10:30)
data AirlineFlightSchedule = AirlineFlightSchedule
  { air_flightschedule_boarding_time  :: Maybe Text -- Boarding time in departure airport timezone
  , air_flightschedule_departure_time :: Text       -- Departure time in departure airport timezone
  , air_flightschedule_arrival_time   :: Text       -- Arrival time in arrival airport timezone
  } deriving (Eq, Show)
-- Must all be in the ISO 8601-based format YYYY-MM-DDThh:mm (e.g. 2015-09-26T10:30)

data AirlineUpdatePassFlightSchedule = AirlineUpdatePassFlightSchedule
  { air_usflightschedule_boarding_time  :: Maybe Text -- Boarding time in departure airport timezone
  , air_usflightschedule_departure_time :: Text       -- Departure time in departure airport timezone
  , air_usflightschedule_arrival_time   :: Maybe Text -- Arrival time in arrival airport timezone
  } deriving (Eq, Show)
-- Must all be in the ISO 8601-based format YYYY-MM-DDThh:mm (e.g. 2015-09-26T10:30)

data AirlinePriceInfo = AirlinePriceInfo
  { air_priceinfo_title    :: Text       -- Price info title
  , air_priceinfo_amount   :: Double     -- Price amount
  , air_priceinfo_currency :: Maybe Text -- Pricing currency (must be ISO-4217-3 code)
  } deriving (Eq, Show)
-- https://developers.facebook.com/docs/payments/reference/supportedcurrencies

data AirlineFlightInfo = AirlineFlightInfo
  { air_cflightinfo_flight_number     :: Text                  -- Flight number
  , air_cflightinfo_departure_airport :: AirlineAirport        -- Departure airport
  , air_cflightinfo_arrival_airport   :: AirlineAirport        -- Arrival airport
  , air_cflightinfo_flight_schedule   :: AirlineFlightSchedule -- Schedule for the flight
  } deriving (Eq, Show)

data AirlineBoardingPass =
  AirlineBoardingPassQRCode
    { air_boardingpass_passenger_name           :: Text       -- Flight number
    , air_boardingpass_pnr_number               :: Text       -- Passenger name record number (Booking Number)
    , air_boardingpass_travel_class             :: Maybe AirlineTravelClassType -- Travel class (ECONOMY, BUSINESS, FIRST_CLASS)
    , air_boardingpass_seat                     :: Maybe Text -- Seat number for passenger
    , air_boardingpass_auxiliary_fields         :: [AirlineField] -- Flexible information to display in the auxiliary section (limited to 5)
    , air_boardingpass_secondary_fields         :: [AirlineField] -- Flexible information to display in the secondary section (limited to 5)
    , air_boardingpass_logo_image_url           :: Text       -- URL for the logo image
    , air_boardingpass_header_image_url         :: Maybe Text -- URL for the header image
    , air_boardingpass_header_text_field        :: Maybe Text -- Text for the header field
    , air_boardingpass_qr_code                  :: Text       -- Aztec or QR code
    , air_boardingpass_above_bar_code_image_url :: Text       -- URL of thin image above the barcode
    , air_boardingpass_flight_info              :: AirlineFlightInfo -- Information about the flight
    }
  | AirlineBoardingPassBarcode
    { air_boardingpass_passenger_name           :: Text       -- Flight number
    , air_boardingpass_pnr_number               :: Text       -- Passenger name record number (Booking Number)
    , air_boardingpass_travel_class             :: Maybe AirlineTravelClassType -- Travel class (ECONOMY, BUSINESS, FIRST_CLASS)
    , air_boardingpass_seat                     :: Maybe Text -- Seat number for passenger
    , air_boardingpass_auxiliary_fields         :: [AirlineField] -- Flexible information to display in the auxiliary section (limited to 5)
    , air_boardingpass_secondary_fields         :: [AirlineField] -- Flexible information to display in the secondary section (limited to 5)
    , air_boardingpass_logo_image_url           :: Text       -- URL for the logo image
    , air_boardingpass_header_image_url         :: Maybe Text -- URL for the header image
    , air_boardingpass_header_text_field        :: Maybe Text -- Text for the header field
    , air_boardingpass_barcode_image_url        :: Text       -- URL of the barcode image
    , air_boardingpass_above_bar_code_image_url :: Text       -- URL of thin image above the barcode
    , air_boardingpass_flight_info              :: AirlineFlightInfo -- Information about the flight
    }
  deriving (Eq, Show)

data AirlinePassengerSegmentInfo = AirlinePassengerSegmentInfo
  { air_passengersegment_segment_id   :: Text -- Used to identify a flight segment (must be unique in itinerary)
  , air_passengersegment_passenger_id :: Text -- `passenger_id` of `passenger_info` object
  , air_passengersegment_seat         :: Text -- Seat number for the passenger
  , air_passengersegment_seat_type    :: Text -- Seat type for the passenger (e.g. Economy comfort)
  , air_passengersegment_product_info :: [AirlineProductInfo] -- List of products the passenger purchased (limited to 4)
  } deriving (Eq, Show)

data AirlineField = AirlineField
  { air_field_label :: Text -- Label for the additional field
  , air_field_value :: Text -- Value for the additional field
  } deriving (Eq, Show)

data AirlineProductInfo = AirlineProductInfo
  { air_product_title :: Text -- Label for the additional field
  , air_product_value :: Text -- Value for the additional field
  } deriving (Eq, Show)


-- ----------------------- --
--    AIRLINE INSTANCES    --
-- ----------------------- --

instance ToJSON AirlinePassengerInfo where
  toJSON (AirlinePassengerInfo ident ticket name) =
    object' [ "passenger_id"  .=! ident
            , "ticket_number" .=!! ticket
            , "name"          .=! name
            ]

instance ToJSON AirlineItineraryFlightInfo where
  toJSON AirlineItineraryFlightInfo{..} =
    object' [ "connection_id"     .=! air_iflightinfo_connection_id
            , "segment_id"        .=! air_iflightinfo_segment_id
            , "flight_number"     .=! air_iflightinfo_flight_number
            , "aircraft_type"     .=!! air_iflightinfo_aircraft_type
            , "departure_airport" .=! air_iflightinfo_departure_airport
            , "arrival_airport"   .=! air_iflightinfo_arrival_airport
            , "flight_schedule"   .=! air_iflightinfo_flight_schedule
            , "travel_class"      .=! air_iflightinfo_travel_class
            ]

instance ToJSON AirlineAirport where
  toJSON (AirlineAirport code city terminal gate) =
    object' [ "airport_code" .=! code
            , "city"         .=! city
            , "terminal"     .=!! terminal
            , "gate"         .=!! gate
            ]

instance ToJSON AirlineFlightSchedule where
  toJSON (AirlineFlightSchedule boarding departure arrival) =
    object' [ "boarding_time"  .=!! boarding
            , "departure_time" .=! departure
            , "arrival_time"   .=! arrival
            ]

instance ToJSON AirlineUpdatePassFlightSchedule where
  toJSON (AirlineUpdatePassFlightSchedule boarding departure arrival) =
    object' [ "boarding_time"  .=!! boarding
            , "departure_time" .=! departure
            , "arrival_time"   .=!! arrival
           ]

instance ToJSON AirlinePriceInfo where
  toJSON (AirlinePriceInfo title amount currency) =
    object' [ "title"    .=! title
            , "amount"   .=! amount
            , "currency" .=!! currency
            ]

instance ToJSON AirlinePassengerSegmentInfo where
  toJSON AirlinePassengerSegmentInfo{..} =
    object' [ "segment_id"   .=! air_passengersegment_segment_id
            , "passenger_id" .=! air_passengersegment_passenger_id
            , "seat"         .=! air_passengersegment_seat
            , "seat_type"    .=! air_passengersegment_seat_type
            , mEmptyList "product_info" $ take 4 air_passengersegment_product_info
            ]

instance ToJSON AirlineFlightInfo where
  toJSON (AirlineFlightInfo number depart arrive schedule) =
    object [ "flight_number"     .= number
           , "departure_airport" .= depart
           , "arrival_airport"   .= arrive
           , "flight_schedule"   .= schedule
           ]

instance ToJSON AirlineBoardingPass where
  toJSON airBoardPass = object' $ extra : basis
   where
    extra = case airBoardPass of
      qr@AirlineBoardingPassQRCode{}  -> "qr_code"           .=! air_boardingpass_qr_code qr 
      bc@AirlineBoardingPassBarcode{} -> "barcode_image_url" .=! air_boardingpass_barcode_image_url bc
    basis = [ "passenger_name"           .=!  air_boardingpass_passenger_name           airBoardPass
            , "pnr_number"               .=!  air_boardingpass_pnr_number               airBoardPass
            , "travel_class"             .=!! air_boardingpass_travel_class             airBoardPass
            , "seat"                     .=!! air_boardingpass_seat                     airBoardPass
            , "logo_image_url"           .=!  air_boardingpass_logo_image_url           airBoardPass
            , "header_image_url"         .=!! air_boardingpass_header_image_url         airBoardPass
            , "header_text_field"        .=!! air_boardingpass_header_text_field        airBoardPass
            , "above_bar_code_image_url" .=!  air_boardingpass_above_bar_code_image_url airBoardPass
            , "flight_info"              .=!  air_boardingpass_flight_info              airBoardPass
            , mEmptyList "auxiliary_fields" $ take 5 $ air_boardingpass_auxiliary_fields  airBoardPass
            , mEmptyList "secondary_fields" $ take 5 $ air_boardingpass_secondary_fields  airBoardPass
            ]


instance ToJSON AirlineField where
  toJSON (AirlineField label value) =
    object [ "label" .= label
           , "value" .= value
           ]

instance ToJSON AirlineProductInfo where
  toJSON (AirlineProductInfo title value) =
    object [ "title" .= title
           , "value" .= value
           ]

-- ---------------------------- --
--  FromJSON AIRLINE INSTANCES  --
-- ---------------------------- --

instance FromJSON AirlinePassengerInfo where
  parseJSON = withObject "AirlinePassengerInfo" $ \o ->
    AirlinePassengerInfo <$> o .: "passenger_id"
                         <*> o .:? "ticket_number"
                         <*> o .: "name"

instance FromJSON AirlineItineraryFlightInfo where
  parseJSON = withObject "AirlineItineraryFlightInfo" $ \o ->
    AirlineItineraryFlightInfo <$> o .: "connection_id"
                               <*> o .: "segment_id"
                               <*> o .: "flight_number"
                               <*> o .:? "aircraft_type"
                               <*> o .: "departure_airport"
                               <*> o .: "arrival_airport"
                               <*> o .: "flight_schedule"
                               <*> o .: "travel_class"

instance FromJSON AirlineAirport where
  parseJSON = withObject "AirlineAirport" $ \o ->
    AirlineAirport <$> o .: "airport_code"
                   <*> o .: "city"
                   <*> o .:? "terminal"
                   <*> o .:? "gate"

instance FromJSON AirlineFlightSchedule where
  parseJSON = withObject "AirlineFlightSchedule" $ \o ->
    AirlineFlightSchedule <$> o .:? "boarding_time"
                          <*> o .: "departure_time"
                          <*> o .: "arrival_time"

instance FromJSON AirlineUpdatePassFlightSchedule where
  parseJSON = withObject "AirlineUpdatePassFlightSchedule" $ \o ->
    AirlineUpdatePassFlightSchedule <$> o .:? "boarding_time"
                                    <*> o .: "departure_time"
                                    <*> o .:? "arrival_time"

instance FromJSON AirlinePriceInfo where
  parseJSON = withObject "AirlinePriceInfo" $ \o ->
    AirlinePriceInfo <$> o .: "title"
                     <*> o .: "amount"
                     <*> o .:? "currency"

instance FromJSON AirlinePassengerSegmentInfo where
  parseJSON = withObject "AirlinePassengerSegmentInfo" $ \o ->
    AirlinePassengerSegmentInfo <$> o .: "segment_id"
                                <*> o .: "passenger_id"
                                <*> o .: "seat"
                                <*> o .: "seat_type"
                                <*> o .:? "product_info" .!= []

instance FromJSON AirlineFlightInfo where
  parseJSON = withObject "AirlineFlightInfo" $ \o ->
    AirlineFlightInfo <$> o .: "flight_number"
                      <*> o .: "departure_airport"
                      <*> o .: "arrival_airport"
                      <*> o .: "flight_schedule"

instance FromJSON AirlineBoardingPass where
  parseJSON = withObject "AirlineBoardingPass" $ \o -> do
    let mQRCode = "qr_code" `HM.lookup` o
        mBarCode = "barcode_image_url" `HM.lookup` o
        eitherQrBar = case (mQRCode,mBarCode) of
          (Just _,Nothing) -> o .: "qr_code"           
          (Nothing,Just _) -> o .: "barcode_image_url" 
          _ -> fail "AirlineBoardingPass needs either qr_code or barcode_image_url"
        boardPass = case mQRCode of
          Just _ -> AirlineBoardingPassQRCode
          _      -> AirlineBoardingPassBarcode
    boardPass <$> o .: "passenger_name"
              <*> o .: "pnr_number"
              <*> o .:? "travel_class"
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

instance FromJSON AirlineProductInfo where
  parseJSON = withObject "AirlineProductInfo" $ \o ->
    AirlineProductInfo <$> o .: "title"
                       <*> o .: "value"
