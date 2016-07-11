{-# LANGUAGE PatternGuards #-}

module Web.Facebook.Messenger.Types.Requests.Airline where

import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)

import Web.Facebook.Messenger.Types.Static


-- ------------------ --
--  AIRLINE REQUESTS  --
-- ------------------ --

data FBAirlineRequestPayload =
  FBAirlineItineraryPayload
    { fbair_itinerary_intro_message          :: Text       -- Introduction message
    , fbair_itinerary_locale                 :: Text
  -- ISO 639-1 language code and a ISO 3166-1 alpha-2 region code (e.g. en_US)
  -- See this document (https://developers.facebook.com/docs/internationalization#locales) for FB's accepted locales
    , fbair_itinerary_theme_color            :: Maybe Text -- Background color of the attachment (RGB hexadecimal string. default #009ddc)
    , fbair_itinerary_pnr_number             :: Text       -- Passenger name record number (Booking Number)
    , fbair_itinerary_passenger_info         :: [FBAirlinePassengerInfo]        -- Information about a passenger
    , fbair_itinerary_flight_info            :: [FBAirlineItineraryFlightInfo]  -- Information about a flight
    , fbair_itinerary_passenger_segment_info :: [FBAirlinePassengerSegmentInfo] -- Information unique to passenger/segment pair
    , fbair_itinerary_price_info             :: Maybe [FBAirlinePriceInfo]      -- Itemization of the total price (limited to 4)
    , fbair_itinerary_base_price             :: Maybe Double -- Base price amount
    , fbair_itinerary_tax                    :: Maybe Double -- Tax amount
    , fbair_itinerary_total_price            :: Double       -- Total price for the booking
    , fbair_itinerary_currency               :: Text
  -- Pricing currency, must be a three digit ISO-4217-3 code (e.g. USD)
  -- (https://developers.facebook.com/docs/payments/reference/supportedcurrencies)
    }
  | FBAirlineCheckinPayload
    { fbair_checkin_intro_message :: Text -- Introduction message
    , fbair_checkin_locale        :: Text -- (e.g. en_US)
    , fbair_checkin_theme_color   :: Maybe Text -- (default #009ddc)
    , fbair_checkin_pnr_number    :: Text -- Passenger name record number (Booking Number)
    , fbair_checkin_flight_info   :: FBAirlineFlightInfo -- Information about a flight
    , fbair_checkin_checkin_url   :: Text -- URL for passengers to check-in
    }
  | FBAirlineBoardingPassPayload
    { fbair_bpass_intro_message :: Text -- Introduction message
    , fbair_bpass_locale        :: Text -- (e.g. en_US)
    , fbair_bpass_theme_color   :: Maybe Text -- (default #009ddc)
    , fbair_bpass_boarding_pass :: FBAirlineBoardingPass -- Boarding passes for passengers
    }
  | FBAirlineFlightUpdateMessagePayload
    { fbair_flightupdate_intro_message      :: Text -- Introduction message
    , fbair_flightupdate_locale             :: Text -- (e.g. en_US)
    , fbair_flightupdate_theme_color        :: Maybe Text -- (default #009ddc)
    , fbair_flightupdate_pnr_number         :: Text -- Passenger name record number (Booking Number)
    , fbair_flightupdate_update_flight_info :: FBAirlineFlightInfo -- Information about a flight
    }
  | FBAirlineFlightUpdateTypePayload
    { fbair_flightupdate_update_type        :: FBAirlineUpdateType -- DELAY, GATE_CHANGE or CANCELLATION
    , fbair_flightupdate_locale             :: Text -- (e.g. en_US)
    , fbair_flightupdate_theme_color        :: Maybe Text -- RGB hexadecimal string (default #009ddc)
    , fbair_flightupdate_pnr_number         :: Text -- Passenger name record number (Booking Number)
    , fbair_flightupdate_update_flight_info :: FBAirlineFlightInfo -- Information about a flight
    }

data FBAirlinePassengerInfo = FBAirlinePassengerInfo
    { fbair_personalinfo_passenger_id  :: Text -- must be unique in itinerary
    , fbair_personalinfo_ticket_number :: Maybe Text -- Ticket number
    , fbair_personalinfo_name          :: Text -- Full name of passenger, including title
    }

data FBAirlineItineraryFlightInfo = FBAirlineItineraryFlightInfo
    { fbair_iflightinfo_connection_id     :: Text -- Used to group segments of a connection together (must be unique in itinerary)
    , fbair_iflightinfo_segment_id        :: Text -- `segment_id` of `passenger_segment_info` object
    , fbair_iflightinfo_flight_number     :: Text -- Flight number
    , fbair_iflightinfo_aircraft_type     :: Maybe Text -- Aircraft type (e.g. Boeing 787)
    , fbair_iflightinfo_departure_airport :: FBAirlineAirport -- Departure airport
    , fbair_iflightinfo_arrival_airport   :: FBAirlineAirport -- Arrival airport
    , fbair_iflightinfo_flight_schedule   :: FBAirlineFlightSchedule  -- Schedule for the flight
    , fbair_iflightinfo_travel_class      :: FBAirlineTravelClassType -- Travel class (ECONOMY, BUSINESS or FIRST_CLASS)
    }

data FBAirlineAirport = FBAirlineAirport
    { fbair_airport_airport_code :: Text -- Airport code (e.g. SFO/AMS/TYO)
    , fbair_airport_city         :: Text -- City name
    , fbair_airport_terminal     :: Maybe Text -- Terminal number
    , fbair_airport_gate         :: Maybe Text -- Gate number
    }

-- all times in schedule must be in the ISO 8601-based format YYYY-MM-DDThh:mm (e.g. 2015-09-26T10:30)
data FBAirlineFlightSchedule = FBAirlineFlightSchedule
    { fbair_flightschedule_boarding_time  :: Maybe Text -- Boarding time in departure airport timezone
    , fbair_flightschedule_departure_time :: Text -- Departure time in departure airport timezone
    , fbair_flightschedule_arrival_time   :: Text -- Arrival time in arrival airport timezone
    }
  -- Must all be in the ISO 8601-based format YYYY-MM-DDThh:mm (e.g. 2015-09-26T10:30)

data FBAirlineUpdatePassFlightSchedule = FBAirlineUpdatePassFlightSchedule
    { fbair_usflightschedule_boarding_time  :: Maybe Text -- Boarding time in departure airport timezone
    , fbair_usflightschedule_departure_time :: Text       -- Departure time in departure airport timezone
    , fbair_usflightschedule_arrival_time   :: Maybe Text -- Arrival time in arrival airport timezone
    }
  -- Must all be in the ISO 8601-based format YYYY-MM-DDThh:mm (e.g. 2015-09-26T10:30)

data FBAirlinePriceInfo = FBAirlinePriceInfo
    { fbair_priceinfo_title    :: Text   -- Price info title
    , fbair_priceinfo_amount   :: Double -- Price amount
    , fbair_priceinfo_currency :: Maybe Text -- Pricing currency (must be ISO-4217-3 code)
    }
  -- https://developers.facebook.com/docs/payments/reference/supportedcurrencies

data FBAirlineFlightInfo = FBAirlineFlightInfo
    { fbair_cflightinfo_flight_number     :: Text             -- Flight number
    , fbair_cflightinfo_departure_airport :: FBAirlineAirport -- Departure airport
    , fbair_cflightinfo_arrival_airport   :: FBAirlineAirport -- Arrival airport
    , fbair_cflightinfo_flight_schedule   :: FBAirlineFlightSchedule -- Schedule for the flight
    }

data FBAirlineBoardingPass =
  FBAirlineBoardingPassQRCode
    { fbair_boardingpass_passenger_name           :: Text -- Flight number
    , fbair_boardingpass_pnr_number               :: Text -- Passenger name record number (Booking Number)
    , fbair_boardingpass_travel_class             :: Maybe FBAirlineTravelClassType -- Travel class (ECONOMY, BUSINESS, FIRST_CLASS)
    , fbair_boardingpass_seat                     :: Maybe Text -- Seat number for passenger
    , fbair_boardingpass_auxiliary_fields         :: Maybe [FBAirlineField] -- Flexible information to display in the auxiliary section (limited to 5)
    , fbair_boardingpass_secondary_fields         :: Maybe [FBAirlineField] -- Flexible information to display in the secondary section (limited to 5)
    , fbair_boardingpass_logo_image_url           :: Text -- URL for the logo image
    , fbair_boardingpass_header_image_url         :: Maybe Text -- URL for the header image
    , fbair_boardingpass_header_text_field        :: Maybe Text -- Text for the header field
    , fbair_boardingpass_qr_code                  :: Text -- Aztec or QR code
    , fbair_boardingpass_above_bar_code_image_url :: Text -- URL of thin image above the barcode
    , fbair_boardingpass_flight_info              :: FBAirlineFlightInfo -- Information about the flight
    }
  | FBAirlineBoardingPassBarcode
    { fbair_boardingpass_passenger_name           :: Text -- Flight number
    , fbair_boardingpass_pnr_number               :: Text -- Passenger name record number (Booking Number)
    , fbair_boardingpass_travel_class             :: Maybe FBAirlineTravelClassType -- Travel class (ECONOMY, BUSINESS, FIRST_CLASS)
    , fbair_boardingpass_seat                     :: Maybe Text -- Seat number for passenger
    , fbair_boardingpass_auxiliary_fields         :: Maybe [FBAirlineField] -- Flexible information to display in the auxiliary section (limited to 5)
    , fbair_boardingpass_secondary_fields         :: Maybe [FBAirlineField] -- Flexible information to display in the secondary section (limited to 5)
    , fbair_boardingpass_logo_image_url           :: Text -- URL for the logo image
    , fbair_boardingpass_header_image_url         :: Maybe Text -- URL for the header image
    , fbair_boardingpass_header_text_field        :: Maybe Text -- Text for the header field
    , fbair_boardingpass_barcode_image_url        :: Text -- URL of the barcode image
    , fbair_boardingpass_above_bar_code_image_url :: Text -- URL of thin image above the barcode
    , fbair_boardingpass_flight_info              :: FBAirlineFlightInfo -- Information about the flight
    }

data FBAirlinePassengerSegmentInfo = FBAirlinePassengerSegmentInfo
    { fbair_passengersegment_segment_id   :: Text -- Used to identify a flight segment (must be unique in itinerary)
    , fbair_passengersegment_passenger_id :: Text -- `passenger_id` of `passenger_info` object
    , fbair_passengersegment_seat         :: Text -- Seat number for the passenger
    , fbair_passengersegment_seat_type    :: Text -- Seat type for the passenger (e.g. Economy comfort)
    , fbair_passengersegment_product_info :: Text -- List of products the passenger purchased (limited to 4)
    }

data FBAirlineField = FBAirlineField
    { fbair_field_label :: Text -- Label for the additional field
    , fbair_field_value :: Text -- Value for the additional field
    }


-- ----------------------- --
--    AIRLINE INSTANCES    --
-- ----------------------- --

instance ToJSON FBAirlineRequestPayload where
    toJSON (FBAirlineItineraryPayload intro locale theme pnr pinfo finfo psinfo
                                      priceinfo bprice tax totalprice currency) = object [ "template_type" .= String "airline_itinerary"
                                                                                         , "intro_message" .= intro
                                                                                         , "locale" .= locale
                                                                                         , "theme_color" .= theme
                                                                                         , "pnr_number" .= pnr
                                                                                         , "passenger_info" .= pinfo
                                                                                         , "flight_info" .= finfo
                                                                                         , "passenger_segment_info" .= psinfo
                                                                                         , "price_info" .= priceinfo
                                                                                         , "base_price" .= bprice
                                                                                         , "tax" .= tax
                                                                                         , "total_price" .= totalprice
                                                                                         , "currency" .= currency
                                                                                         ]
    toJSON (FBAirlineCheckinPayload intro locale theme pnr finfo checkin) = object [ "template_type" .= String "airline_checkin"
                                                                                   , "intro_message" .= intro
                                                                                   , "locale" .= locale
                                                                                   , "theme_color" .= theme
                                                                                   , "pnr_number" .= pnr
                                                                                   , "flight_info" .= finfo
                                                                                   , "checkin_url" .= checkin
                                                                                   ]
    toJSON (FBAirlineBoardingPassPayload intro locale theme boarding) = object [ "template_type" .= String "airline_boardingpass"
                                                                               , "intro_message" .= intro
                                                                               , "locale" .= locale
                                                                               , "theme_color" .= theme
                                                                               , "boarding_pass" .= boarding
                                                                               ]
    toJSON (FBAirlineFlightUpdateMessagePayload intro locale theme pnr update') = object [ "template_type" .= String "airline_update"
                                                                                        , "intro_message" .= intro
                                                                                        , "locale" .= locale
                                                                                        , "theme_color" .= theme
                                                                                        , "pnr_number" .= pnr
                                                                                        , "update_flight_info" .= update'
                                                                                        ]
    toJSON (FBAirlineFlightUpdateTypePayload typ locale theme pnr update') = object [ "template_type" .= String "airline_update"
                                                                                   , "update_type" .= typ
                                                                                   , "locale" .= locale
                                                                                   , "theme_color" .= theme
                                                                                   , "pnr_number" .= pnr
                                                                                   , "update_flight_info" .= update'
                                                                                   ]

instance ToJSON FBAirlinePassengerInfo where
    toJSON (FBAirlinePassengerInfo ident ticket name) = object [ "passenger_id" .= ident
                                                               , "ticket_number" .= ticket
                                                               , "name" .= name
                                                               ] 

instance ToJSON FBAirlineItineraryFlightInfo where
    toJSON (FBAirlineItineraryFlightInfo connection segment number aircraft dep arriv schedule travel) = object [ "connection_id" .= connection
                                                                                                                , "segment_id" .= segment
                                                                                                                , "flight_number" .= number
                                                                                                                , "aircraft_type" .= aircraft
                                                                                                                , "departure_airport" .= dep
                                                                                                                , "arrival_airport" .= arriv
                                                                                                                , "flight_schedule" .= schedule
                                                                                                                , "travel_class" .= travel
                                                                                                                ]

instance ToJSON FBAirlineAirport where
    toJSON (FBAirlineAirport code city terminal gate) = object [ "airport_code" .= code
                                                               , "city" .= city
                                                               , "terminal" .= terminal
                                                               , "gate" .= gate
                                                               ]

instance ToJSON FBAirlineFlightSchedule where
    toJSON (FBAirlineFlightSchedule boarding departure arrival) = object [ "boarding_time" .= boarding
                                                                         , "departure_time" .= departure
                                                                         , "arrival_time" .= arrival
                                                                         ]

instance ToJSON FBAirlineUpdatePassFlightSchedule where
    toJSON (FBAirlineUpdatePassFlightSchedule boarding departure arrival) = object [ "boarding_time" .= boarding
                                                                                   , "departure_time" .= departure
                                                                                   , "arrival_time" .= arrival
                                                                                   ]

instance ToJSON FBAirlinePriceInfo where
    toJSON (FBAirlinePriceInfo title amount currency) = object [ "title" .= title
                                                               , "amount" .= amount
                                                               , "currency" .= currency
                                                               ]

instance ToJSON FBAirlinePassengerSegmentInfo where
    toJSON (FBAirlinePassengerSegmentInfo segment passenger seat typ pinfo) = object [ "segment_id" .= segment
                                                                                     , "passenger_id" .= passenger
                                                                                     , "seat" .= seat
                                                                                     , "seat_type" .= typ
                                                                                     , "product_info" .= pinfo
                                                                                     ]

instance ToJSON FBAirlineFlightInfo where
    toJSON (FBAirlineFlightInfo number depart arrive schedule) = object [ "flight_number" .= number
                                                                        , "departure_airport" .= depart
                                                                        , "arrival_airport" .= arrive
                                                                        , "flight_schedule" .= schedule
                                                                        ]

instance ToJSON FBAirlineBoardingPass where
    toJSON (FBAirlineBoardingPassQRCode passenger pnr travel seat aux secondary 
                                        logo headerurl headertext qr aboveimg finfo) = object [ "passenger_name" .= passenger
                                                                                              , "pnr_number" .= pnr
                                                                                              , "travel_class" .= travel
                                                                                              , "seat" .= seat
                                                                                              , "auxiliary_fields" .= aux
                                                                                              , "secondary_fields" .= secondary
                                                                                              , "logo_image_url" .= logo
                                                                                              , "header_image_url" .= headerurl
                                                                                              , "header_text_field" .= headertext
                                                                                              , "qr_code" .= qr
                                                                                              , "above_bar_code_image_url" .= aboveimg
                                                                                              , "flight_info" .= finfo
                                                                                              ]
    toJSON (FBAirlineBoardingPassBarcode passenger pnr travel seat aux secondary logo
                                         headerurl headertext barcode aboveimg finfo) = object [ "passenger_name" .= passenger
                                                                                               , "pnr_number" .= pnr
                                                                                               , "travel_class" .= travel
                                                                                               , "seat" .= seat
                                                                                               , "auxiliary_fields" .= aux
                                                                                               , "secondary_fields" .= secondary
                                                                                               , "logo_image_url" .= logo
                                                                                               , "header_image_url" .= headerurl
                                                                                               , "header_text_field" .= headertext
                                                                                               , "barcode_image_url" .= barcode
                                                                                               , "above_bar_code_image_url" .= aboveimg
                                                                                               , "flight_info" .= finfo
                                                                                               ]

instance ToJSON FBAirlineField where
    toJSON (FBAirlineField label value) = object [ "label" .= label
                                                 , "value" .= value
                                                 ]


-- ---------------------------- --
--  FromJSON AIRLINE INSTANCES  --
-- ---------------------------- --

instance FromJSON FBAirlineRequestPayload where
    parseJSON (Object o) = FBAirlineFlightUpdateTypePayload <$> o .: "update_type"
                                                            <*> o .: "locale"
                                                            <*> o .:? "theme_color"
                                                            <*> o .: "pnr_number"
                                                            <*> o .: "update_flight_info"
                       <|> FBAirlineBoardingPassPayload <$> o .: "intro_message"
                                                        <*> o .: "locale"
                                                        <*> o .:? "theme_color"
                                                        <*> o .: "boarding_pass"
                       <|> FBAirlineFlightUpdateMessagePayload <$> o .: "intro_message"
                                                               <*> o .: "locale"
                                                               <*> o .:? "theme_color"
                                                               <*> o .: "pnr_number"
                                                               <*> o .: "update_flight_info"
                       <|> FBAirlineCheckinPayload <$> o .: "intro_message"
                                                   <*> o .: "locale"
                                                   <*> o .:? "theme_color"
                                                   <*> o .: "pnr_number"
                                                   <*> o .: "flight_info"
                                                   <*> o .: "checkin_url"
                       <|> FBAirlineItineraryPayload <$> o .: "intro_message"
                                                     <*> o .: "locale"
                                                     <*> o .:? "theme_color"
                                                     <*> o .: "pnr_number"
                                                     <*> o .: "passenger_info"
                                                     <*> o .: "flight_info"
                                                     <*> o .: "passenger_segment_info"
                                                     <*> o .:? "price_info"
                                                     <*> o .:? "base_price"
                                                     <*> o .:? "tax"
                                                     <*> o .: "total_price"
                                                     <*> o .: "currency"
    parseJSON wat = typeMismatch "FBAirlineRequestPayload" wat

instance FromJSON FBAirlinePassengerInfo where
    parseJSON (Object o) = FBAirlinePassengerInfo <$> o .: "passenger_id"
                                                  <*> o .:? "ticket_number"
                                                  <*> o .: "name"
    parseJSON wat = typeMismatch "FBAirlinePassengerInfo" wat

instance FromJSON FBAirlineItineraryFlightInfo where
    parseJSON (Object o) = FBAirlineItineraryFlightInfo <$> o .: "connection_id"
                                                        <*> o .: "segment_id"
                                                        <*> o .: "flight_number"
                                                        <*> o .:? "aircraft_type"
                                                        <*> o .: "departure_airport"
                                                        <*> o .: "arrival_airport"
                                                        <*> o .: "flight_schedule"
                                                        <*> o .: "travel_class"
    parseJSON wat = typeMismatch "FBAirlineItineraryFlightInfo" wat

instance FromJSON FBAirlineAirport where
    parseJSON (Object o) = FBAirlineAirport <$> o .: "airport_code"
                                            <*> o .: "city"
                                            <*> o .:? "terminal"
                                            <*> o .:? "gate"
    parseJSON wat = typeMismatch "FBAirlineAirport" wat

instance FromJSON FBAirlineFlightSchedule where
    parseJSON (Object o) = FBAirlineFlightSchedule <$> o .:? "boarding_time"
                                                   <*> o .: "departure_time"
                                                   <*> o .: "arrival_time"
    parseJSON wat = typeMismatch "FBAirlineFlightSchedule" wat

instance FromJSON FBAirlineUpdatePassFlightSchedule where
    parseJSON (Object o) = FBAirlineUpdatePassFlightSchedule <$> o .:? "boarding_time"
                                                             <*> o .: "departure_time"
                                                             <*> o .:? "arrival_time"
    parseJSON wat = typeMismatch "FBAirlineUpdatePassFlightSchedule" wat

instance FromJSON FBAirlinePriceInfo where
    parseJSON (Object o) = FBAirlinePriceInfo <$> o .: "title"
                                              <*> o .: "amount"
                                              <*> o .:? "currency"
    parseJSON wat = typeMismatch "FBAirlinePriceInfo" wat

instance FromJSON FBAirlinePassengerSegmentInfo where
    parseJSON (Object o) = FBAirlinePassengerSegmentInfo <$> o .: "segment_id"
                                                         <*> o .: "passenger_id"
                                                         <*> o .: "seat"
                                                         <*> o .: "seat_type"
                                                         <*> o .: "product_info"
    parseJSON wat = typeMismatch "FBAirlinePassengerSegmentInfo" wat

instance FromJSON FBAirlineFlightInfo where
    parseJSON (Object o) = FBAirlineFlightInfo <$> o .: "flight_number"
                                               <*> o .: "departure_airport"
                                               <*> o .: "arrival_airport"
                                               <*> o .: "flight_schedule"
    parseJSON wat = typeMismatch "FBAirlineFlightInfo" wat

instance FromJSON FBAirlineBoardingPass where
    parseJSON (Object o) = FBAirlineBoardingPassQRCode <$> o .: "passenger_name"
                                                       <*> o .: "pnr_number"
                                                       <*> o .:? "travel_class"
                                                       <*> o .:? "seat"
                                                       <*> o .:? "auxiliary_fields"
                                                       <*> o .:? "secondary_fields"
                                                       <*> o .: "logo_image_url"
                                                       <*> o .:? "header_image_url"
                                                       <*> o .:? "header_text_field"
                                                       <*> o .: "qr_code"
                                                       <*> o .: "above_bar_code_image_url"
                                                       <*> o .: "flight_info"
                       <|> FBAirlineBoardingPassBarcode <$> o .: "passenger_name"
                                                        <*> o .: "pnr_number"
                                                        <*> o .:? "travel_class"
                                                        <*> o .:? "seat"
                                                        <*> o .:? "auxiliary_fields"
                                                        <*> o .:? "secondary_fields"
                                                        <*> o .: "logo_image_url"
                                                        <*> o .:? "header_image_url"
                                                        <*> o .:? "header_text_field"
                                                        <*> o .: "barcode_image_url"
                                                        <*> o .: "above_bar_code_image_url"
                                                        <*> o .: "flight_info"
    parseJSON wat = typeMismatch "FBAirlineBoardingPass" wat

instance FromJSON FBAirlineField where
    parseJSON (Object o) = FBAirlineField <$> o .: "label"
                                          <*> o .: "value"
    parseJSON wat = typeMismatch "FBAirlineField" wat
