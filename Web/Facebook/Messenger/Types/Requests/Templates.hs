{-# LANGUAGE PatternGuards #-}

module Web.Facebook.Messenger.Types.Requests.Templates where

import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)

import qualified Data.HashMap.Strict        as HM

import Web.Facebook.Messenger.Types.Static            (AirlineUpdateType)
import Web.Facebook.Messenger.Types.Requests.Airline
-- ------------------ --
--  TEMPLATE REQUEST  --
-- ------------------ --

data TemplatePayload = GenericTemplatePayload
                                { template_generic_elements :: [GenericTemplateElement] }
                                  -- Data for each bubble in message (10 bubble limit)
                             | ButtonTemplatePayload
                                { template_button_text    :: Text -- Text that appears in main body (UTF8 320 char limit)
                                , template_button_buttons :: [TemplateButton] }
                            -- Set of buttons that appear as call-to-actions (3 buttons limit)
                             | ReceiptTemplatePayload
                                { template_receipt_recipient_name :: Text -- Recipient's name
                                , template_receipt_order_number   :: Text -- Order number (MUST BE UNIQUE)
                                , template_receipt_currency       :: Text -- Currency for order
                                , template_receipt_payment_method :: Text -- Payment method details. This can be a custom string. ex: "Visa 1234".
                                , template_receipt_timestamp      :: Maybe Text -- Timestamp of order
                                , template_receipt_order_url      :: Maybe Text -- URL of order
                                , template_receipt_elements       :: [ReceiptTemplateElement]
                            -- Items in order (max 100, sort order not quaranteed)
                                , template_receipt_address        :: Maybe TemplateAddress -- Shipping address (optional)
                                , template_receipt_summary        :: TemplateSummary -- Payment summary
                                , template_receipt_adjustments    :: Maybe [TemplateAdjustment]
                            -- Payment adjustments (allow a way to insert adjusted pricing (e.g., sales))
                                }
                             | AirlineItineraryPayload
                                { air_itinerary_intro_message          :: Text       -- Introduction message
                                , air_itinerary_locale                 :: Text
                              -- ISO 639-1 language code and a ISO 3166-1 alpha-2 region code (e.g. en_US)
                              -- See this document (https://developers.facebook.com/docs/internationalization#locales) for FB's accepted locales
                                , air_itinerary_theme_color            :: Maybe Text -- Background color of the attachment (RGB hexadecimal string. default #009ddc)
                                , air_itinerary_pnr_number             :: Text       -- Passenger name record number (Booking Number)
                                , air_itinerary_passenger_info         :: [AirlinePassengerInfo]        -- Information about a passenger
                                , air_itinerary_flight_info            :: [AirlineItineraryFlightInfo]  -- Information about a flight
                                , air_itinerary_passenger_segment_info :: [AirlinePassengerSegmentInfo] -- Information unique to passenger/segment pair
                                , air_itinerary_price_info             :: Maybe [AirlinePriceInfo]      -- Itemization of the total price (limited to 4)
                                , air_itinerary_base_price             :: Maybe Double -- Base price amount
                                , air_itinerary_tax                    :: Maybe Double -- Tax amount
                                , air_itinerary_total_price            :: Double       -- Total price for the booking
                                , air_itinerary_currency               :: Text
                              -- Pricing currency, must be a three digit ISO-4217-3 code (e.g. USD)
                              -- (https://developers.facebook.com/docs/payments/reference/supportedcurrencies)
                                }
                             | AirlineCheckinPayload
                                { air_checkin_intro_message :: Text -- Introduction message
                                , air_checkin_locale        :: Text -- (e.g. en_US)
                                , air_checkin_theme_color   :: Maybe Text -- (default #009ddc)
                                , air_checkin_pnr_number    :: Text -- Passenger name record number (Booking Number)
                                , air_checkin_flight_info   :: AirlineFlightInfo -- Information about a flight
                                , air_checkin_checkin_url   :: Text -- URL for passengers to check-in
                                }
                             | AirlineBoardingPassPayload
                                { air_bpass_intro_message :: Text -- Introduction message
                                , air_bpass_locale        :: Text -- (e.g. en_US)
                                , air_bpass_theme_color   :: Maybe Text -- (default #009ddc)
                                , air_bpass_boarding_pass :: AirlineBoardingPass -- Boarding passes for passengers
                                }
                             | AirlineFlightUpdateMessagePayload
                                { air_flightupdate_intro_message      :: Text -- Introduction message
                                , air_flightupdate_locale             :: Text -- (e.g. en_US)
                                , air_flightupdate_theme_color        :: Maybe Text -- (default #009ddc)
                                , air_flightupdate_pnr_number         :: Text -- Passenger name record number (Booking Number)
                                , air_flightupdate_update_flight_info :: AirlineFlightInfo -- Information about a flight
                                }
                              | AirlineFlightUpdateTypePayload
                                { air_flightupdate_update_type        :: AirlineUpdateType -- DELAY, GATE_CHANGE or CANCELLATION
                                , air_flightupdate_locale             :: Text -- (e.g. en_US)
                                , air_flightupdate_theme_color        :: Maybe Text -- RGB hexadecimal string (default #009ddc)
                                , air_flightupdate_pnr_number         :: Text -- Passenger name record number (Booking Number)
                                , air_flightupdate_update_flight_info :: AirlineFlightInfo -- Information about a flight
                                }
  deriving (Eq, Show)


data GenericTemplateElement = GenericTemplateElement
    { generic_template_title     :: Text       -- Bubble title (80 char limit)
    , generic_template_item_url  :: Maybe Text -- URL that is opened when bubble is tapped
    , generic_template_image_url :: Maybe Text -- Bubble image (1.91:1 image ratio)
    , generic_template_subtitle  :: Maybe Text -- Bubble subtitle (80 char limit)
    , generic_template_buttons   :: Maybe [TemplateButton] -- Set of buttons that appear as call-to-actions (3 button limit)
    }
  deriving (Eq, Show)

data ReceiptTemplateElement = ReceiptTemplateElement
    { receipt_template_title     :: Text       -- Title of item
    , receipt_template_subtitle  :: Maybe Text -- Subtitle of item
    , receipt_template_quantity  :: Maybe Int  -- Quantity of item
    , receipt_template_price     :: Double     -- Item price (0 is allowed)
    , receipt_template_currency  :: Maybe Text -- Currency of price
    , receipt_template_image_url :: Maybe Text -- 1.91:1 image ratio
    }
  deriving (Eq, Show)

data TemplateButton = TemplateButtonWebURL { button_title      :: Text -- 20 char limimt
                                           , button_weburl_url :: Text }
                                    -- This URL is opened in a mobile browser when the button is tapped
                    | TemplateButtonPostback { button_title            :: Text -- 20 char limimt
                                             , button_postback_payload :: Text } -- 1000 char limit
                                    -- This data will be sent back to you via webhook.
                    | TemplateButtonPhoneNumber { button_title         :: Text -- 20 char limit
                                                , button_phone_payload :: Text }
                                    -- This must be a well formatted phone number. (+31654321098 or +31(6)54321098 ?)
  deriving (Eq, Show)

data TemplateAddress = TemplateAddress
    { address_template_street_1    :: Text       -- Street address, line 1
    , address_template_street_2    :: Maybe Text -- Street address, line 2
    , address_template_city        :: Text       -- City
    , address_template_postal_code :: Text       -- Postal code
    , address_template_state       :: Text       -- State abbreviation
    , address_template_country     :: Text       -- Two-letter country abbreviation
    }
  deriving (Eq, Show)

data TemplateSummary = TemplateSummary
    { summary_template_subtotal      :: Maybe Double -- Subtotal
    , summary_template_shipping_cost :: Maybe Double -- Cost of shipping
    , summary_template_total_tax     :: Maybe Double -- Total tax
    , summary_template_total_cost    :: Double       -- Total cost
    }
  deriving (Eq, Show)

data TemplateAdjustment = TemplateAdjustment
    { adjustment_template_name   :: Maybe Text -- Name of adjustment
    , adjustment_template_amount :: Maybe Int  -- Adjustment amount
    }
  deriving (Eq, Show)


-- -------------------- --
--  TEMPLATE INSTANCES  --
-- -------------------- --

instance ToJSON TemplatePayload where
    toJSON (GenericTemplatePayload elements) = object [ "template_type" .= String "generic"
                                                      , "elements" .= elements
                                                      ]
    toJSON (ButtonTemplatePayload text buttons) = object [ "template_type" .= String "button"
                                                         , "text" .= text
                                                         , "buttons" .= buttons
                                                         ]
    toJSON (ReceiptTemplatePayload recipient_name order_number currency payment_method
                                   timestamp      order_url    elements address
                                   summary        adjustments) = object [ "template_type" .= String "receipt"
                                                                        , "recipient_name" .= recipient_name
                                                                        , "order_number" .= order_number
                                                                        , "currency" .= currency
                                                                        , "payment_method" .= payment_method
                                                                        , "timestamp" .= timestamp
                                                                        , "order_url" .= order_url
                                                                        , "elements" .= elements
                                                                        , "address" .= address
                                                                        , "summary" .= summary
                                                                        , "adjustments" .= adjustments
                                                                        ]
    toJSON (AirlineItineraryPayload intro  locale theme      pnr
                                    pinfo  finfo  psinfo     priceinfo
                                    bprice tax    totalprice currency) = object [ "template_type" .= String "airline_itinerary"
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
    toJSON (AirlineCheckinPayload intro locale theme pnr finfo checkin) = object [ "template_type" .= String "airline_checkin"
                                                                                 , "intro_message" .= intro
                                                                                 , "locale" .= locale
                                                                                 , "theme_color" .= theme
                                                                                 , "pnr_number" .= pnr
                                                                                 , "flight_info" .= finfo
                                                                                 , "checkin_url" .= checkin
                                                                                 ]
    toJSON (AirlineBoardingPassPayload intro locale theme boarding) = object [ "template_type" .= String "airline_boardingpass"
                                                                             , "intro_message" .= intro
                                                                             , "locale" .= locale
                                                                             , "theme_color" .= theme
                                                                             , "boarding_pass" .= boarding
                                                                             ]
    toJSON (AirlineFlightUpdateMessagePayload intro locale theme pnr update') = object [ "template_type" .= String "airline_update"
                                                                                       , "intro_message" .= intro
                                                                                       , "locale" .= locale
                                                                                       , "theme_color" .= theme
                                                                                       , "pnr_number" .= pnr
                                                                                       , "update_flight_info" .= update'
                                                                                       ]
    toJSON (AirlineFlightUpdateTypePayload typ locale theme pnr update') = object [ "template_type" .= String "airline_update"
                                                                                  , "update_type" .= typ
                                                                                  , "locale" .= locale
                                                                                  , "theme_color" .= theme
                                                                                  , "pnr_number" .= pnr
                                                                                  , "update_flight_info" .= update'
                                                                                  ]

instance ToJSON GenericTemplateElement where
    toJSON (GenericTemplateElement title item_url image_url subtitle buttons) = object [ "title" .= title
                                                                                       , "item_url" .= item_url
                                                                                       , "image_url" .= image_url
                                                                                       , "subtitle" .= subtitle
                                                                                       , "buttons" .= buttons
                                                                                       ]

instance ToJSON ReceiptTemplateElement where
    toJSON (ReceiptTemplateElement title subtitle quantity price currency image_url) = object [ "title" .= title
                                                                                              , "subtitle" .= subtitle
                                                                                              , "quantity" .= quantity
                                                                                              , "price" .= price
                                                                                              , "currency" .= currency
                                                                                              , "image_url" .= image_url
                                                                                              ]

instance ToJSON TemplateButton where
    toJSON (TemplateButtonWebURL title url) = object [ "type" .= String "web_url"
                                                     , "title" .= title
                                                     , "url" .= url
                                                     ]
    toJSON (TemplateButtonPostback title payload) = object [ "type" .= String "postback"
                                                           , "title" .= title
                                                           , "payload" .= payload
                                                           ]
    toJSON (TemplateButtonPhoneNumber title payload) = object [ "type" .= String "phone_number"
                                                              , "title" .= title
                                                              , "payload" .= payload
                                                              ]

instance ToJSON TemplateAddress where
    toJSON (TemplateAddress street_1 street_2 city postal_code state country) = object [ "street_1" .= street_1
                                                                                       , "street_2" .= street_2
                                                                                       , "city" .= city
                                                                                       , "postal_code" .= postal_code
                                                                                       , "state" .= state
                                                                                       , "country" .= country
                                                                                       ]

instance ToJSON TemplateSummary where
    toJSON (TemplateSummary subtotal shipping_cost total_tax total_cost) = object [ "subtotal" .= subtotal
                                                                                  , "shipping_cost" .= shipping_cost
                                                                                  , "total_tax" .= total_tax
                                                                                  , "total_cost" .= total_cost
                                                                                  ]

instance ToJSON TemplateAdjustment where
    toJSON (TemplateAdjustment name amount) = object [ "name" .= name
                                                     , "amount" .= amount
                                                     ]


instance FromJSON TemplatePayload where
    parseJSON (Object o) = GenericTemplatePayload <$> o .: "elements"
                       <|> ButtonTemplatePayload <$> o .: "text"
                                                 <*> o .: "buttons"
                       <|> ReceiptTemplatePayload <$> o .: "recipient_name"
                                                  <*> o .: "order_number"
                                                  <*> o .: "currency"
                                                  <*> o .: "payment_method"
                                                  <*> o .:? "timestamp"
                                                  <*> o .:? "order_url"
                                                  <*> o .: "elements"
                                                  <*> o .:? "address"
                                                  <*> o .: "summary"
                                                  <*> o .:? "adjustments"
                       <|> AirlineFlightUpdateTypePayload <$> o .: "update_type"
                                                          <*> o .: "locale"
                                                          <*> o .:? "theme_color"
                                                          <*> o .: "pnr_number"
                                                          <*> o .: "update_flight_info"
                       <|> AirlineBoardingPassPayload <$> o .: "intro_message"
                                                      <*> o .: "locale"
                                                      <*> o .:? "theme_color"
                                                      <*> o .: "boarding_pass"
                       <|> AirlineFlightUpdateMessagePayload <$> o .: "intro_message"
                                                             <*> o .: "locale"
                                                             <*> o .:? "theme_color"
                                                             <*> o .: "pnr_number"
                                                             <*> o .: "update_flight_info"
                       <|> AirlineCheckinPayload <$> o .: "intro_message"
                                                 <*> o .: "locale"
                                                 <*> o .:? "theme_color"
                                                 <*> o .: "pnr_number"
                                                 <*> o .: "flight_info"
                                                 <*> o .: "checkin_url"
                       <|> AirlineItineraryPayload <$> o .: "intro_message"
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
    parseJSON wat = typeMismatch "TemplatePayload" wat

instance FromJSON GenericTemplateElement where
    parseJSON (Object o) = GenericTemplateElement <$> o .: "title"
                                                  <*> o .:? "item_url"
                                                  <*> o .:? "image_url"
                                                  <*> o .:? "subtitle"
                                                  <*> o .:? "buttons"
    parseJSON wat = typeMismatch "GenericTemplateElement" wat

instance FromJSON ReceiptTemplateElement where
    parseJSON (Object o) = ReceiptTemplateElement <$> o .: "title"
                                                  <*> o .:? "subtitle"
                                                  <*> o .:? "quantity"
                                                  <*> o .: "price"
                                                  <*> o .:? "currency"
                                                  <*> o .:? "image_url"
    parseJSON wat = typeMismatch "ReceiptTemplateElement" wat

instance FromJSON TemplateButton where
    parseJSON (Object o) | Just typ <- HM.lookup "type" o,
                           typ == "phone_number" = TemplateButtonPhoneNumber <$> o .: "title"
                                                                             <*> o .: "payload"
                         | otherwise             = TemplateButtonWebURL <$> o .: "title"
                                                                        <*> o .: "url"
                                               <|> TemplateButtonPostback <$> o .: "title"
                                                                          <*> o .: "payload"
    parseJSON wat = typeMismatch "TemplateButton" wat

instance FromJSON TemplateAddress where
    parseJSON (Object o) = TemplateAddress <$> o .: "street_1"
                                           <*> o .:? "street_2"
                                           <*> o .: "city"
                                           <*> o .: "postal_code"
                                           <*> o .: "state"
                                           <*> o .: "country"
    parseJSON wat = typeMismatch "TemplateAddress" wat

instance FromJSON TemplateSummary where
    parseJSON (Object o) = TemplateSummary <$> o .:? "subtotal"
                                           <*> o .:? "shipping_cost"
                                           <*> o .:? "total_tax"
                                           <*> o .: "total_cost"
    parseJSON wat = typeMismatch "TemplateSummary" wat

instance FromJSON TemplateAdjustment where
    parseJSON (Object o) = TemplateAdjustment <$> o .:? "name"
                                              <*> o .:? "amount"
    parseJSON wat = typeMismatch "TemplateAdjustment" wat
