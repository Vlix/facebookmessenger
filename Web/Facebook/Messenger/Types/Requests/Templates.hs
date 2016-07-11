{-# LANGUAGE PatternGuards #-}

module Web.Facebook.Messenger.Types.Requests.Templates where

import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)

import qualified Data.HashMap.Strict        as HM


-- ------------------ --
--  TEMPLATE REQUEST  --
-- ------------------ --

data FBRequestTemplatePayload = FBRequestGenericTemplatePayload
                                { fbreq_template_generic_elements :: [FBRequestGenericTemplateElement] }
                                  -- Data for each bubble in message (10 bubble limit)
                             | FBRequestButtonTemplatePayload
                                { fbreq_template_button_text    :: Text -- Text that appears in main body (UTF8 320 char limit)
                                , fbreq_template_button_buttons :: [FBRequestTemplateButton] }
                            -- Set of buttons that appear as call-to-actions (3 buttons limit)
                             | FBRequestReceiptTemplatePayload
                                { fbreq_template_receipt_recipient_name :: Text -- Recipient's name
                                , fbreq_template_receipt_order_number   :: Text -- Order number (MUST BE UNIQUE)
                                , fbreq_template_receipt_currency       :: Text -- Currency for order
                                , fbreq_template_receipt_payment_method :: Text -- Payment method details. This can be a custom string. ex: "Visa 1234".
                                , fbreq_template_receipt_timestamp      :: Maybe Text -- Timestamp of order
                                , fbreq_template_receipt_order_url      :: Maybe Text -- URL of order
                                , fbreq_template_receipt_elements       :: [FBRequestReceiptTemplateElement]
                            -- Items in order (max 100, sort order not quaranteed)
                                , fbreq_template_receipt_address        :: Maybe FBRequestTemplateAddress -- Shipping address (optional)
                                , fbreq_template_receipt_summary        :: FBRequestTemplateSummary -- Payment summary
                                , fbreq_template_receipt_adjustments    :: Maybe [FBRequestTemplateAdjustment]
                            -- Payment adjustments (allow a way to insert adjusted pricing (e.g., sales))
                                }

data FBRequestGenericTemplateElement = FBRequestGenericTemplateElement
    { fbreq_generic_template_title     :: Text       -- Bubble title (80 char limit)
    , fbreq_generic_template_item_url  :: Maybe Text -- URL that is opened when bubble is tapped
    , fbreq_generic_template_image_url :: Maybe Text -- Bubble image (1.91:1 image ratio)
    , fbreq_generic_template_subtitle  :: Maybe Text -- Bubble subtitle (80 char limit)
    , fbreq_generic_template_buttons   :: Maybe [FBRequestTemplateButton] -- Set of buttons that appear as call-to-actions (3 button limit)
    }

data FBRequestReceiptTemplateElement = FBRequestReceiptTemplateElement
    { fbreq_receipt_template_title     :: Text       -- Title of item
    , fbreq_receipt_template_subtitle  :: Maybe Text -- Subtitle of item
    , fbreq_receipt_template_quantity  :: Maybe Int  -- Quantity of item
    , fbreq_receipt_template_price     :: Double     -- Item price (0 is allowed)
    , fbreq_receipt_template_currency  :: Maybe Text -- Currency of price
    , fbreq_receipt_template_image_url :: Maybe Text -- 1.91:1 image ratio
    }

data FBRequestTemplateButton = FBRequestTemplateButtonWebURL { fbreq_button_weburl_title :: Text -- 20 char limimt
                                                             , fbreq_button_weburl_url   :: Text }
                                                          -- This URL is opened in a mobile browser when the button is tapped
                             | FBRequestTemplateButtonPostback { fbreq_button_postback_title   :: Text -- 20 char limimt
                                                               , fbreq_button_postback_payload :: Text } -- 1000 char limit
                                                          -- This data will be sent back to you via webhook.
                             | FBRequestTemplateButtonPhoneNumber { fbreq_button_phone_title   :: Text -- 20 char limit
                                                                  , fbreq_button_phone_payload :: Text }
                                                          -- This must be a well formatted phone number. (+31654321098 or +31(6)54321098 ?)

data FBRequestTemplateAddress = FBRequestTemplateAddress
    { fbreq_address_template_street_1    :: Text       -- Street address, line 1
    , fbreq_address_template_street_2    :: Maybe Text -- Street address, line 2
    , fbreq_address_template_city        :: Text       -- City
    , fbreq_address_template_postal_code :: Text       -- Postal code
    , fbreq_address_template_state       :: Text       -- State abbreviation
    , fbreq_address_template_country     :: Text       -- Two-letter country abbreviation
    }

data FBRequestTemplateSummary = FBRequestTemplateSummary
    { fbreq_summary_template_subtotal      :: Maybe Double -- Subtotal
    , fbreq_summary_template_shipping_cost :: Maybe Double -- Cost of shipping
    , fbreq_summary_template_total_tax     :: Maybe Double -- Total tax
    , fbreq_summary_template_total_cost    :: Double       -- Total cost
    }

data FBRequestTemplateAdjustment = FBRequestTemplateAdjustment
    { fbreq_adjustment_template_name   :: Maybe Text -- Name of adjustment
    , fbreq_adjustment_template_amount :: Maybe Int  -- Adjustment amount
    }


-- -------------------- --
--  TEMPLATE INSTANCES  --
-- -------------------- --

instance ToJSON FBRequestTemplatePayload where
    toJSON (FBRequestGenericTemplatePayload elements) = object [ "template_type" .= String "generic"
                                                               , "elements" .= elements
                                                               ]
    toJSON (FBRequestButtonTemplatePayload text buttons) = object [ "template_type" .= String "button"
                                                                  , "text" .= text
                                                                  , "buttons" .= buttons
                                                                  ]
    toJSON (FBRequestReceiptTemplatePayload recipient_name order_number currency payment_method timestamp
            order_url elements address summary adjustments) = object [ "template_type" .= String "receipt"
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

instance ToJSON FBRequestGenericTemplateElement where
    toJSON (FBRequestGenericTemplateElement title item_url image_url subtitle buttons) = object [ "title" .= title
                                                                                                , "item_url" .= item_url
                                                                                                , "image_url" .= image_url
                                                                                                , "subtitle" .= subtitle
                                                                                                , "buttons" .= buttons
                                                                                                ]

instance ToJSON FBRequestReceiptTemplateElement where
    toJSON (FBRequestReceiptTemplateElement title subtitle quantity price currency image_url) = object [ "title" .= title
                                                                                                       , "subtitle" .= subtitle
                                                                                                       , "quantity" .= quantity
                                                                                                       , "price" .= price
                                                                                                       , "currency" .= currency
                                                                                                       , "image_url" .= image_url
                                                                                                       ]

instance ToJSON FBRequestTemplateButton where
    toJSON (FBRequestTemplateButtonWebURL title url) = object [ "type" .= String "web_url"
                                                              , "title" .= title
                                                              , "url" .= url ]
    toJSON (FBRequestTemplateButtonPostback title payload) = object [ "type" .= String "postback"
                                                                    , "title" .= title
                                                                    , "payload" .= payload ]
    toJSON (FBRequestTemplateButtonPhoneNumber title payload) = object [ "type" .= String "phone_number"
                                                                       , "title" .= title
                                                                       , "payload" .= payload ]

instance ToJSON FBRequestTemplateAddress where
    toJSON (FBRequestTemplateAddress street_1 street_2 city postal_code state country) = object [ "street_1" .= street_1
                                                                                                , "street_2" .= street_2
                                                                                                , "city" .= city
                                                                                                , "postal_code" .= postal_code
                                                                                                , "state" .= state
                                                                                                , "country" .= country
                                                                                                ]

instance ToJSON FBRequestTemplateSummary where
    toJSON (FBRequestTemplateSummary subtotal shipping_cost total_tax total_cost) = object [ "subtotal" .= subtotal
                                                                                           , "shipping_cost" .= shipping_cost
                                                                                           , "total_tax" .= total_tax
                                                                                           , "total_cost" .= total_cost
                                                                                           ]

instance ToJSON FBRequestTemplateAdjustment where
    toJSON (FBRequestTemplateAdjustment name amount) = object [ "name" .= name
                                                              , "amount" .= amount
                                                              ]


instance FromJSON FBRequestTemplatePayload where
    parseJSON (Object o) = FBRequestGenericTemplatePayload <$> o .: "elements"
                       <|> FBRequestButtonTemplatePayload <$> o .: "text"
                                                          <*> o .: "buttons"
                       <|> FBRequestReceiptTemplatePayload <$> o .: "recipient_name"
                                                           <*> o .: "order_number"
                                                           <*> o .: "currency"
                                                           <*> o .: "payment_method"
                                                           <*> o .:? "timestamp"
                                                           <*> o .:? "order_url"
                                                           <*> o .: "elements"
                                                           <*> o .:? "address"
                                                           <*> o .: "summary"
                                                           <*> o .:? "adjustments"
    parseJSON wat = typeMismatch "FBRequestTemplatePayload" wat

instance FromJSON FBRequestGenericTemplateElement where
    parseJSON (Object o) = FBRequestGenericTemplateElement <$> o .: "title"
                                                           <*> o .:? "item_url"
                                                           <*> o .:? "image_url"
                                                           <*> o .:? "subtitle"
                                                           <*> o .:? "buttons"
    parseJSON wat = typeMismatch "FBRequestGenericTemplateElement" wat

instance FromJSON FBRequestReceiptTemplateElement where
    parseJSON (Object o) = FBRequestReceiptTemplateElement <$> o .: "title"
                                                           <*> o .:? "subtitle"
                                                           <*> o .:? "quantity"
                                                           <*> o .: "price"
                                                           <*> o .:? "currency"
                                                           <*> o .:? "image_url"
    parseJSON wat = typeMismatch "FBRequestReceiptTemplateElement" wat

instance FromJSON FBRequestTemplateButton where
    parseJSON (Object o) | Just typ <- HM.lookup "type" o,
                           typ == "phone_number" = FBRequestTemplateButtonPhoneNumber <$> o .: "title"
                                                                                      <*> o .: "payload"
                         | otherwise             = FBRequestTemplateButtonWebURL <$> o .: "title"
                                                                                 <*> o .: "url"
                                               <|> FBRequestTemplateButtonPostback <$> o .: "title"
                                                                                   <*> o .: "payload"
    parseJSON wat = typeMismatch "FBRequestTemplateButton" wat

instance FromJSON FBRequestTemplateAddress where
    parseJSON (Object o) = FBRequestTemplateAddress <$> o .: "street_1"
                                                    <*> o .:? "street_2"
                                                    <*> o .: "city"
                                                    <*> o .: "postal_code"
                                                    <*> o .: "state"
                                                    <*> o .: "country"
    parseJSON wat = typeMismatch "FBRequestTemplateAddress" wat

instance FromJSON FBRequestTemplateSummary where
    parseJSON (Object o) = FBRequestTemplateSummary <$> o .:? "subtotal"
                                                    <*> o .:? "shipping_cost"
                                                    <*> o .:? "total_tax"
                                                    <*> o .: "total_cost"
    parseJSON wat = typeMismatch "FBRequestTemplateSummary" wat

instance FromJSON FBRequestTemplateAdjustment where
    parseJSON (Object o) = FBRequestTemplateAdjustment <$> o .:? "name"
                                                       <*> o .:? "amount"
    parseJSON wat = typeMismatch "FBRequestTemplateAdjustment" wat
