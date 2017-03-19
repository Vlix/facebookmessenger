{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Facebook.Messenger.Types.Requests.Templates where

import           Control.Applicative  ((<|>))
import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text)
import qualified Data.Vector          as V

import           Web.Facebook.Messenger.Types.Requests.Airline
import           Web.Facebook.Messenger.Types.Static

-- ------------------ --
--  TEMPLATE REQUEST  --
-- ------------------ --

data TemplatePayload =
  GenericTemplatePayload
    { template_generic_elements    :: [GenericTemplateElement] }
  -- Data for each bubble in message (10 bubble limit)
  | ButtonTemplatePayload
    { template_button_text         :: Text -- Text that appears in main body (UTF8 320 char limit)
    , template_button_buttons      :: [TemplateButton]
    }
  -- Set of buttons that appear as call-to-actions (3 buttons limit)
  | ListTemplatePayload
    { template_list_top_element_style :: ListStyle             -- Value must be large or compact. Default to large if not specified
    , template_list_elements          :: [ListTemplateElement] -- List view elements (maximum of 4 elements and minimum of 2 elements)
    , template_list_button            :: Maybe TemplateButton  -- `List` of buttons associated on the list template message (maximum of 1 button).
    }
  | ReceiptTemplatePayload
    { template_receipt_recipient_name :: Text       -- Recipient's name
    , template_receipt_merchants_name :: Maybe Text -- If present this is shown as logo text.
    , template_receipt_order_number   :: Text       -- Order number (MUST BE UNIQUE)
    , template_receipt_currency       :: Text       -- Currency for order
    , template_receipt_payment_method :: Text       -- Payment method details. This can be a custom string. ex: "Visa 1234".
    , template_receipt_timestamp      :: Maybe Text -- Timestamp of order in seconds (I guess POSIXtime?)
    , template_receipt_order_url      :: Maybe Text -- URL of order
    , template_receipt_elements       :: [ReceiptTemplateElement]
  -- Items in order (max 100, sort order not quaranteed)
    , template_receipt_address        :: Maybe TemplateAddress -- Shipping address (optional)
    , template_receipt_summary        :: TemplateSummary       -- Payment summary
    , template_receipt_adjustments    :: [TemplateAdjustment]
  -- Payment adjustments (allow a way to insert adjusted pricing (e.g., sales))
    }
  | AirlineItineraryPayload
    { air_itinerary_intro_message          :: Text         -- Introduction message
    , air_itinerary_locale                 :: Text
  -- ISO 639-1 language code and a ISO 3166-1 alpha-2 region code (e.g. en_US)
  -- See this document (https://developers.facebook.com/docs/internationalization#locales) for FB's accepted locales
    , air_itinerary_theme_color            :: Maybe Text   -- Background color of the attachment (RGB hexadecimal string. default #009ddc)
    , air_itinerary_pnr_number             :: Text         -- Passenger name record number (Booking Number)
    , air_itinerary_passenger_info         :: [AirlinePassengerInfo]        -- Information about a passenger
    , air_itinerary_flight_info            :: [AirlineItineraryFlightInfo]  -- Information about a flight
    , air_itinerary_passenger_segment_info :: [AirlinePassengerSegmentInfo] -- Information unique to passenger/segment pair
    , air_itinerary_price_info             :: [AirlinePriceInfo]            -- Itemization of the total price (limited to 4)
    , air_itinerary_base_price             :: Maybe Double -- Base price amount
    , air_itinerary_tax                    :: Maybe Double -- Tax amount
    , air_itinerary_total_price            :: Double       -- Total price for the booking
    , air_itinerary_currency               :: Text
  -- Pricing currency, must be a three digit ISO-4217-3 code (e.g. USD)
  -- (https://developers.facebook.com/docs/payments/reference/supportedcurrencies)
    }
  | AirlineCheckinPayload
    { air_checkin_intro_message :: Text       -- Introduction message
    , air_checkin_locale        :: Text       -- (e.g. en_US)
    , air_checkin_theme_color   :: Maybe Text -- (default #009ddc)
    , air_checkin_pnr_number    :: Text       -- Passenger name record number (Booking Number)
    , air_checkin_flight_info   :: [AirlineFlightInfo] -- Information about a flight
    , air_checkin_checkin_url   :: Text       -- URL for passengers to check-in
    }
  | AirlineBoardingPassPayload
    { air_bpass_intro_message :: Text                  -- Introduction message
    , air_bpass_locale        :: Text                  -- (e.g. en_US)
    , air_bpass_theme_color   :: Maybe Text            -- (default #009ddc)
    , air_bpass_boarding_pass :: [AirlineBoardingPass] -- Boarding passes for passengers
    }
  | AirlineFlightUpdatePayload
    { air_flightupdate_intro_message      :: Maybe Text        -- Introduction message
    , air_flightupdate_update_type        :: AirlineUpdateType -- DELAY, GATE_CHANGE or CANCELLATION
    , air_flightupdate_locale             :: Text              -- (e.g. en_US)
    , air_flightupdate_theme_color        :: Maybe Text        -- RGB hexadecimal string (default #009ddc)
    , air_flightupdate_pnr_number         :: Text              -- Passenger name record number (Booking Number)
    , air_flightupdate_update_flight_info :: AirlineFlightInfo -- Information about a flight
    }
  deriving (Eq, Show)


data GenericTemplateElement =
  GenericTemplateElement
    { generic_template_title     :: Text       -- Bubble title (80 char limit)
    , generic_template_either_url_action :: Maybe (Either Text DefaultAction)
    -- URL that is opened when bubble is tapped OR default action to be triggered when user taps on the element
    , generic_template_image_url :: Maybe Text -- Bubble image (1.91:1 image ratio)
    , generic_template_subtitle  :: Maybe Text -- Bubble subtitle (80 char limit)
    , generic_template_buttons   :: [TemplateButton] -- Set of buttons that appear as call-to-actions (3 button limit)
    }
  | GenericBuyTemplateElement
    { generic_template_title      :: Text       -- Bubble title (80 char limit)
    , generic_template_either_url_action :: Maybe (Either Text DefaultAction)
    -- URL that is opened when bubble is tapped OR default action to be triggered when user taps on the element
    , generic_template_image_url  :: Maybe Text -- Bubble image (1.91:1 image ratio)
    , generic_template_subtitle   :: Maybe Text -- Bubble subtitle (80 char limit)
    , generic_template_buy_button :: BUYBUTTON  -- This MUST be a TemplateBuyButton !!!
    , generic_template_buttons    :: [TemplateButton] -- Set of buttons that appear as call-to-actions (2 button limit)
    }
  deriving (Eq, Show)

newtype BUYBUTTON = BUYBUTTON { getBUYBUTTON :: TemplateButton}
  deriving (Eq, Show)

data ListTemplateElement = ListTemplateElement
  { list_template_title     :: Text
  , list_template_subtitle  :: Maybe Text
  , list_template_image_url :: Maybe Text
  , list_template_default_action :: Maybe DefaultAction
  , list_template_buttons   :: Maybe TemplateButton
  } deriving (Eq, Show)

data DefaultAction =
  DefaultAction
    { default_action_url     :: Text -- This URL is opened in a mobile browser when the template is tapped
    , default_action_webview :: Maybe WebViewHeightRatioType -- Height of the Webview. Defaults to `full`.
    }
  | DefaultActionMessengerExtensions
    { default_action_url      :: Text       -- This URL is opened in a mobile browser when the template is tapped
    , default_action_webview  :: Maybe WebViewHeightRatioType -- Height of the Webview. Defaults to `full`.
    , default_action_fallback :: Maybe Text -- URL to use on clients that don't support Messenger Extensions. If this is not defined, the url will be used as the fallback.
    }
  deriving (Eq, Show)

data ReceiptTemplateElement = ReceiptTemplateElement
  { receipt_template_title     :: Text       -- Title of item
  , receipt_template_subtitle  :: Maybe Text -- Subtitle of item
  , receipt_template_quantity  :: Maybe Int  -- Quantity of item
  , receipt_template_price     :: Double     -- Item price (0 is allowed)
  , receipt_template_currency  :: Maybe Text -- Currency of price
  , receipt_template_image_url :: Maybe Text -- 1.91:1 image ratio
  } deriving (Eq, Show)

data TemplateButton =
  TemplateButtonWebURL { button_title            :: Text -- 20 char limimt
                       , button_weburl_url       :: Text -- This URL is opened in a mobile browser when the button is tapped
                       , button_weburl_webview   :: Maybe WebViewHeightRatioType -- Height of the Webview. (default is FULL)
                       }
  | TemplateButtonWebURLMessengerExtension { button_title             :: Text -- 20 char limimt
                                           , button_weburl_url        :: Text -- This URL is opened in a mobile browser when the button is tapped
                                           , button_weburl_webview    :: Maybe WebViewHeightRatioType -- Height of the Webview. (default is FULL)
                                           , button_msgrexts_fallback :: Maybe Text
                                           }
  | TemplateButtonPostback { button_title            :: Text -- 20 char limimt
                           , button_postback_payload :: Text } -- 1000 char limit
                         -- This data will be sent back to you via webhook.
  | TemplateButtonPhoneNumber { button_title         :: Text -- 20 char limit
                              , button_phone_payload :: Text }
                          -- This must be a well formatted phone number. (+31654321098 or +31(6)54321098 ?)
  | TemplateButtonAccountLink { button_url :: Text }
  | TemplateButtonAccountUnlink
  | TemplateShareButton
  | TemplateBuyButton { button_buy_payload         :: Text
                      , button_buy_currency        :: Text
                      , button_buy_is_test_payment :: Bool
                      , button_buy_payment_type    :: PaymentType
                      , button_buy_merchant_name   :: Text
                      , button_buy_requested_user_info :: [RequestedUserInfoType]
                      , button_buy_price_list      :: [PriceObject]
                      }
  deriving (Eq, Show)

data PriceObject = PriceObject { price_label  :: Text
                               , price_amount :: Text
                               }
  deriving (Eq, Show)

data TemplateAddress = TemplateAddress
  { address_template_street_1    :: Text       -- Street address, line 1
  , address_template_street_2    :: Maybe Text -- Street address, line 2
  , address_template_city        :: Text       -- City
  , address_template_postal_code :: Text       -- Postal code
  , address_template_state       :: Text       -- State abbreviation
  , address_template_country     :: Text       -- Two-letter country abbreviation
  } deriving (Eq, Show)

data TemplateSummary = TemplateSummary
  { summary_template_subtotal      :: Maybe Double -- Subtotal
  , summary_template_shipping_cost :: Maybe Double -- Cost of shipping
  , summary_template_total_tax     :: Maybe Double -- Total tax
  , summary_template_total_cost    :: Double       -- Total cost
  } deriving (Eq, Show)

data TemplateAdjustment = TemplateAdjustment
  { adjustment_template_name   :: Maybe Text -- Name of adjustment
  , adjustment_template_amount :: Maybe Int  -- Adjustment amount
  } deriving (Eq, Show)


-- -------------------- --
--  TEMPLATE INSTANCES  --
-- -------------------- --

instance ToJSON TemplatePayload where
  toJSON (GenericTemplatePayload elements) =
    object [ "template_type" .= String "generic"
           , "elements"      .= take 10 elements
           ]
  toJSON (ButtonTemplatePayload text buttons) =
    object [ "template_type" .= String "button"
           , "text"          .= text
           , "buttons"       .= take 3 buttons
           ]
  toJSON (ListTemplatePayload style elements button) =
    object' [ "template_type"     .=! String "list"
            , "top_element_style" .=! style
            , "elements"          .=! go elements
            , "buttons"           .=!! fmap (:[]) button
            ]
    where go [e] = [e,e]
          go es  = take 4 es
  toJSON ReceiptTemplatePayload{..} =
    object' [ "template_type"  .=! String "receipt"
            , "recipient_name" .=! template_receipt_recipient_name
            , "merchant_name"  .=!! template_receipt_merchants_name
            , "order_number"   .=! template_receipt_order_number
            , "currency"       .=! template_receipt_currency
            , "payment_method" .=! template_receipt_payment_method
            , "timestamp"      .=!! template_receipt_timestamp
            , "order_url"      .=!! template_receipt_order_url
            , "elements"       .=! template_receipt_elements
            , "address"        .=!! template_receipt_address
            , "summary"        .=! template_receipt_summary
            , mEmptyList "adjustments" template_receipt_adjustments
            ]
  toJSON AirlineItineraryPayload{..} =
    object' [ "template_type"          .=! String "airline_itinerary"
            , "intro_message"          .=! air_itinerary_intro_message
            , "locale"                 .=! air_itinerary_locale
            , "theme_color"            .=!! air_itinerary_theme_color
            , "pnr_number"             .=! air_itinerary_pnr_number
            , "passenger_info"         .=! air_itinerary_passenger_info
            , "flight_info"            .=! air_itinerary_flight_info
            , "passenger_segment_info" .=! air_itinerary_passenger_segment_info
            , "base_price"             .=!! air_itinerary_base_price
            , "tax"                    .=!! air_itinerary_tax
            , "total_price"            .=! air_itinerary_total_price
            , "currency"               .=! air_itinerary_currency
            , mEmptyList "priceinfo" $ take 4 air_itinerary_price_info
            ]
  toJSON AirlineCheckinPayload{..} =
    object' [ "template_type" .=! String "airline_checkin"
            , "intro_message" .=! air_checkin_intro_message
            , "locale"        .=! air_checkin_locale
            , "theme_color"   .=!! air_checkin_theme_color
            , "pnr_number"    .=! air_checkin_pnr_number
            , "flight_info"   .=! air_checkin_flight_info
            , "checkin_url"   .=! air_checkin_checkin_url
            ]
  toJSON AirlineBoardingPassPayload{..} =
    object' [ "template_type" .=! String "airline_boardingpass"
            , "intro_message" .=! air_bpass_intro_message
            , "locale"        .=! air_bpass_locale
            , "theme_color"   .=!! air_bpass_theme_color
            , "boarding_pass" .=! air_bpass_boarding_pass
            ]
  toJSON AirlineFlightUpdatePayload{..} =
    object' [ "template_type"      .=! String "airline_update"
            , "intro_message"      .=!! air_flightupdate_intro_message
            , "update_type"        .=! air_flightupdate_update_type
            , "locale"             .=! air_flightupdate_locale
            , "theme_color"        .=!! air_flightupdate_theme_color
            , "pnr_number"         .=! air_flightupdate_pnr_number
            , "update_flight_info" .=! air_flightupdate_update_flight_info
            ]

instance ToJSON GenericTemplateElement where
  toJSON GenericTemplateElement{..} =
    object' [ "title"     .=! generic_template_title
            , "image_url" .=!! generic_template_image_url
            , "subtitle"  .=!! generic_template_subtitle
            , go generic_template_either_url_action
            , mEmptyList "buttons" $ take 3 generic_template_buttons
            ]
    where go (Just (Right default_action)) = Just $ "default_action" .= default_action
          go (Just (Left item_url))        = Just $ "item_url"       .= item_url
          go Nothing                       = Nothing
  toJSON GenericBuyTemplateElement{..} =
    object' [ "title"     .=! generic_template_title
            , "image_url" .=!! generic_template_image_url
            , "subtitle"  .=!! generic_template_subtitle
            , "buttons"   .=! (getBUYBUTTON generic_template_buy_button : take 2 generic_template_buttons)
            , go generic_template_either_url_action
            ]
    where go (Just (Right default_action)) = Just $ "default_action" .= default_action
          go (Just (Left item_url))        = Just $ "item_url"       .= item_url
          go Nothing                       = Nothing

instance ToJSON ListTemplateElement where
  toJSON ListTemplateElement{..} =
    object' [ "title"          .=! list_template_title
            , "subtitle"       .=!! list_template_subtitle
            , "image_url"      .=!! list_template_image_url
            , "default_action" .=!! list_template_default_action
            , "buttons"        .=!! fmap (:[]) list_template_buttons
            ]

instance ToJSON DefaultAction where
  toJSON (DefaultAction url webview) =
    object' [ "type" .=! String "web_url"
            , "url"  .=! url
            , "webview_height_ratio" .=!! webview
            ]
  toJSON (DefaultActionMessengerExtensions url webview fallback) =
    object' [ "type"                 .=! String "web_url"
            , "url"                  .=! url
            , "webview_height_ratio" .=!! webview
            , "messenger_extensions" .=! Bool True
            , "fallback_url"         .=!! fallback
            ]

instance ToJSON ReceiptTemplateElement where
    toJSON ReceiptTemplateElement{..} =
        object' [ "title"     .=! receipt_template_title
                , "subtitle"  .=!! receipt_template_subtitle
                , "quantity"  .=!! receipt_template_quantity
                , "price"     .=! receipt_template_price
                , "currency"  .=!! receipt_template_currency
                , "image_url" .=!! receipt_template_image_url
                ]

instance ToJSON TemplateButton where
  toJSON (TemplateButtonWebURL title url webview) =
    object' [ "type"                 .=! String "web_url"
            , "title"                .=! title
            , "url"                  .=! url
            , "webview_height_ratio" .=!! webview
            ]
  toJSON (TemplateButtonWebURLMessengerExtension title url webview fallback) =
    object' [ "type"                 .=! String "web_url"
            , "title"                .=! title
            , "url"                  .=! url
            , "webview_height_ratio" .=!! webview
            , "messenger_extensions" .=! Bool True
            , "fallback_url"         .=!! fallback
            ]
  toJSON (TemplateButtonPostback title payload) =
    object [ "type"    .= String "postback"
           , "title"   .= title
           , "payload" .= payload
           ]
  toJSON (TemplateButtonPhoneNumber title payload) =
    object [ "type"    .= String "phone_number"
           , "title"   .= title
           , "payload" .= payload
           ]
  toJSON (TemplateButtonAccountLink url) =
    object [ "type" .= String "account_link"
           , "url"  .= url
           ]
  toJSON TemplateButtonAccountUnlink = object [ "type" .= String "account_unlink" ]
  toJSON TemplateShareButton         = object [ "type" .= String "element_share" ]
  toJSON TemplateBuyButton{..} =
    object [ "type"    .= String "payment"
           , "title"   .= String "buy"
           , "payload" .= button_buy_payload
           , "payment_summary" .= object' [ "currency"        .=! button_buy_currency
                                          , mBool "is_test_payment" False button_buy_is_test_payment
                                          , "payment_type"    .=! button_buy_payment_type
                                          , "merchant_name"   .=! button_buy_merchant_name
                                          , "requested_user_info" .=! button_buy_requested_user_info
                                          , "price_list"      .=! button_buy_price_list
                                          ]
           ]

instance ToJSON PriceObject where
  toJSON (PriceObject label amount) =
    object [ "label"  .= label
           , "amount" .= amount
           ]

instance ToJSON TemplateAddress where
  toJSON TemplateAddress{..} =
    object' [ "street_1"    .=! address_template_street_1
            , "street_2"    .=!! address_template_street_2
            , "city"        .=! address_template_city
            , "postal_code" .=! address_template_postal_code
            , "state"       .=! address_template_state
            , "country"     .=! address_template_country
            ]

instance ToJSON TemplateSummary where
  toJSON TemplateSummary{..} =
    object' [ "subtotal"      .=!! summary_template_subtotal
            , "shipping_cost" .=!! summary_template_shipping_cost
            , "total_tax"     .=!! summary_template_total_tax
            , "total_cost"    .=! summary_template_total_cost
            ]

instance ToJSON TemplateAdjustment where
  toJSON (TemplateAdjustment name amount) =
    object' [ "name"   .=!! name
            , "amount" .=!! amount
            ]


-- -------------------- --
--  FromJSON Instances  --
-- -------------------- --

instance FromJSON TemplatePayload where
  parseJSON = withObject "TemplatePayload" $ \o ->
        GenericTemplatePayload <$> o .: "elements"
    <|> ButtonTemplatePayload <$> o .: "text"
                              <*> o .: "buttons"
    <|> ReceiptTemplatePayload <$> o .: "recipient_name"
                               <*> o .:? "merchant_name"
                               <*> o .: "order_number"
                               <*> o .: "currency"
                               <*> o .: "payment_method"
                               <*> o .:? "timestamp"
                               <*> o .:? "order_url"
                               <*> o .: "elements"
                               <*> o .:? "address"
                               <*> o .: "summary"
                               <*> o .:? "adjustments" .!= []
    <|> AirlineFlightUpdatePayload <$> o .:? "intro_message"
                                   <*> o .: "update_type"
                                   <*> o .: "locale"
                                   <*> o .:? "theme_color"
                                   <*> o .: "pnr_number"
                                   <*> o .: "update_flight_info"
    <|> AirlineBoardingPassPayload <$> o .: "intro_message"
                                   <*> o .: "locale"
                                   <*> o .:? "theme_color"
                                   <*> o .: "boarding_pass"
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
                                <*> o .:? "price_info" .!= []
                                <*> o .:? "base_price"
                                <*> o .:? "tax"
                                <*> o .: "total_price"
                                <*> o .: "currency"

instance FromJSON GenericTemplateElement where
  parseJSON (Object o)
    | Just (Array a) <- HM.lookup "buttons" o
    , not (V.null a)
    , Object ob <- V.head a =
        case (HM.lookup "type" ob,HM.lookup "default_action" o) of
          (Just (String "payment"),Nothing) ->
            GenericBuyTemplateElement <$> o .: "title"
                                      <*> o .:? "item_url"
                                      <*> o .:? "image_url"
                                      <*> o .:? "subtitle"
                                      <*> (BUYBUTTON <$> parseJSON (Object ob))
                                      <*> if V.length a == 1 then pure [] else parseJSON (Array $ V.tail a)
          (Just (String "payment"),Just (Object _)) ->
            GenericBuyTemplateElement <$> o .: "title"
                                      <*> o .:? "default_action"
                                      <*> o .:? "image_url"
                                      <*> o .:? "subtitle"
                                      <*> (BUYBUTTON <$> parseJSON (Object ob))
                                      <*> if V.length a == 1 then pure [] else parseJSON (Array $ V.tail a)
          _    -> toDefault
    | otherwise = toDefault
   where toDefault = case HM.lookup "default_action" o of
            Just (Object _) ->
                GenericTemplateElement <$> o .: "title"
                                       <*> o .:? "default_action"
                                       <*> o .:? "image_url"
                                       <*> o .:? "subtitle"
                                       <*> o .:? "buttons" .!= []
            _ ->
                GenericTemplateElement <$> o .: "title"
                                       <*> o .:? "item_url"
                                       <*> o .:? "image_url"
                                       <*> o .:? "subtitle"
                                       <*> o .:? "buttons" .!= []
  parseJSON wat = typeMismatch "GenericTemplateElement" wat

instance {-# OVERLAPPING #-} FromJSON (Either Text DefaultAction) where
  parseJSON     (String t) = pure $ Left t
  parseJSON obj@(Object _) = Right <$> parseJSON obj
  parseJSON wat = typeMismatch "Either Text DefaultAction" wat

instance FromJSON ListTemplateElement where
  parseJSON = withObject "ListTemplateElement" $ \o ->
    ListTemplateElement <$> o .: "title"
                        <*> o .:? "subtitle"
                        <*> o .:? "image_url"
                        <*> o .:? "default_action"
                        <*> o .:? "buttons"

instance FromJSON DefaultAction where
  parseJSON = withObject "DefaultAction" $ \o ->
    if "messenger_extensions" `HM.lookup` o == Just (Bool True)
      then DefaultActionMessengerExtensions <$> o .: "url"
                                            <*> o .:? "webview_height_ratio"
                                            <*> o .:? "fallback_url"
      else DefaultAction <$> o .: "url"
                         <*> o .:? "webview_height_ratio"

instance FromJSON ReceiptTemplateElement where
  parseJSON = withObject "ReceiptTemplateElement" $ \o ->
    ReceiptTemplateElement <$> o .: "title"
                           <*> o .:? "subtitle"
                           <*> o .:? "quantity"
                           <*> o .: "price"
                           <*> o .:? "currency"
                           <*> o .:? "image_url"

instance FromJSON TemplateButton where
  parseJSON = withObject "TemplateButton" $ \o ->
    case HM.lookup "type" o of
      Just "element_share"  -> pure TemplateShareButton
      Just "account_unlink" -> pure TemplateButtonAccountUnlink
      Just "account_link"   -> TemplateButtonAccountLink <$> o .: "url"
      Just "phone_number"   -> TemplateButtonPhoneNumber <$> o .: "title"
                                                         <*> o .: "payload"
      Just "postback"       -> TemplateButtonPostback <$> o .: "title"
                                                      <*> o .: "payload"
      Just "web_url"        ->
        if "messenger_extensions" `HM.lookup` o == Just (Bool True)
          then TemplateButtonWebURLMessengerExtension <$> o .: "title"
                                                      <*> o .: "url"
                                                      <*> o .:? "webview_height_ratio"
                                                      <*> o .:? "fallback_url"
          else TemplateButtonWebURL <$> o .: "title"
                                    <*> o .: "url"
                                    <*> o .:? "webview_height_ratio"
      Just "payment"        ->
        case HM.lookup "payment_summary" o of
          Just (Object ob) ->
            TemplateBuyButton <$> o .: "payload"
                              <*> ob .: "currency"
                              <*> ob .:? "is_test_payment" .!= False
                              <*> ob .: "payment_type"
                              <*> ob .: "merchant_name"
                              <*> ob .: "requested_user_info"
                              <*> ob .: "price_list"
          _ -> fail "No required 'payment_summary' field in TemplateButton with type 'payment'"
      _ -> fail "No valid type value in TemplateButton object"

instance FromJSON PriceObject where
  parseJSON = withObject "PriceObject" $ \o ->
    PriceObject <$> o .: "label"
                <*> o .: "amount"

instance FromJSON TemplateAddress where
  parseJSON = withObject "TemplateAddress" $ \o ->
    TemplateAddress <$> o .: "street_1"
                    <*> o .:? "street_2"
                    <*> o .: "city"
                    <*> o .: "postal_code"
                    <*> o .: "state"
                    <*> o .: "country"

instance FromJSON TemplateSummary where
  parseJSON = withObject "TemplateSummary" $ \o ->
    TemplateSummary <$> o .:? "subtotal"
                    <*> o .:? "shipping_cost"
                    <*> o .:? "total_tax"
                    <*> o .: "total_cost"

instance FromJSON TemplateAdjustment where
  parseJSON = withObject "TemplateAdjustment" $ \o ->
    TemplateAdjustment <$> o .:? "name"
                       <*> o .:? "amount"
