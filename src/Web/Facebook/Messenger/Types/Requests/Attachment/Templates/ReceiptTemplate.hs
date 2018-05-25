{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment.Templates.ReceiptTemplate
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Use the Receipt Template with the Send API to send a order confirmation, with the transaction summary and description for each item.

https://developers.facebook.com/docs/messenger-platform/send-api-reference/receipt-template
-}
module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.ReceiptTemplate (
  -- * Receipt Template
  ReceiptTemplate (..)
  , ReceiptElement (..)
  , ReceiptSummary (..)
  , ReceiptAdjustment (..)
  )
where

import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests.Extra (TemplateAddress)
import Web.Facebook.Messenger.Types.Static (URL)


-- -------------------------- --
--  RECEIPT TEMPLATE REQUEST  --
-- -------------------------- --

-- | Template for sending a receipt to a user
data ReceiptTemplate = ReceiptTemplate
    { rtRecipientName :: Text -- ^ Recipient's name
    , rtMerchantsName :: Maybe Text -- ^ If present this is shown as logo text.
    , rtOrderNumber :: Text -- ^ Order number (MUST BE UNIQUE)
    , rtCurrency :: Text
    -- ^ Currency for order
    --
    -- https://developers.facebook.com/docs/payments/reference/supportedcurrencies
    , rtPaymentMethod :: Text -- ^ Payment method details. This can be a custom string. ex: "Visa 1234".
    , rtTimestamp :: Maybe Text -- ^ Timestamp of order in seconds (I guess POSIXtime?)
    , rtOrderUrl :: Maybe URL -- ^ URL of order
    , rtElements :: [ReceiptElement]
    -- ^ Items in order (max 100, sort order not quaranteed)
    , rtAddress :: Maybe TemplateAddress -- ^ Shipping address. If you do not ship an item, you may omit this
    , rtSummary :: ReceiptSummary -- ^ Payment summary
    , rtAdjustments :: [ReceiptAdjustment]
    -- ^ Payment adjustments (allow a way to insert adjusted pricing (e.g., sales))
    , rtSharable :: Bool -- ^ Set to `False` to disable the native share button in Messenger for the template message.
    } deriving (Eq, Show, Read, Ord)

instance ToJSON ReceiptTemplate where
  toJSON rtp =
      object' [ "template_type" .=! String "receipt"
              , "recipient_name" .=! rtRecipientName rtp
              , "merchant_name" .=!! rtMerchantsName rtp
              , "order_number" .=! rtOrderNumber rtp
              , "currency" .=! rtCurrency rtp
              , "payment_method" .=! rtPaymentMethod rtp
              , "timestamp" .=!! rtTimestamp rtp
              , "order_url" .=!! rtOrderUrl rtp
              , mEmptyList "elements" $ rtElements rtp
              , "address" .=!! rtAddress rtp
              , "summary" .=! rtSummary rtp
              , mEmptyList "adjustments" $ rtAdjustments rtp
              , mDefault "sharable" True $ rtSharable rtp
              ]

instance FromJSON ReceiptTemplate where
  parseJSON = checkValue
      "ReceiptTemplate"
      "template_type"
      ("receipt" :: Text)
      $ \o -> ReceiptTemplate <$> o .: "recipient_name"
                              <*> o .:? "merchant_name"
                              <*> o .: "order_number"
                              <*> o .: "currency"
                              <*> o .: "payment_method"
                              <*> o .:? "timestamp"
                              <*> o .:? "order_url"
                              <*> o .:? "elements" .!= []
                              <*> o .:? "address"
                              <*> o .: "summary"
                              <*> o .:? "adjustments" .!= []
                              <*> o .:? "sharable" .!= True

-- | Items in order
data ReceiptElement = ReceiptElement
    { reTitle :: Text -- ^ Title of item
    , reSubtitle :: Maybe Text -- ^ Subtitle of item
    , reQuantity :: Maybe Int -- ^ Quantity of item
    , rePrice :: Double -- ^ Item price (0 is allowed)
    , reCurrency :: Maybe Text -- ^ Currency of price
    , reImageUrl :: Maybe Text -- ^ 1.91:1 image ratio
    } deriving (Eq, Show, Read, Ord)

instance ToJSON ReceiptElement where
  toJSON (ReceiptElement title subtitle quantity price currency image) =
      object' [ "title" .=! title
              , "subtitle" .=!! subtitle
              , "quantity" .=!! quantity
              , "price" .=! price
              , "currency" .=!! currency
              , "image_url" .=!! image
              ]

instance FromJSON ReceiptElement where
  parseJSON = withObject "ReceiptElement" $ \o ->
      ReceiptElement <$> o .: "title"
                     <*> o .:? "subtitle"
                     <*> o .:? "quantity"
                     <*> o .: "price"
                     <*> o .:? "currency"
                     <*> o .:? "image_url"

-- | Payment summary.
--
-- These numbers should be valid and well formatted decimal numbers,
-- using \'.\' (dot) as the decimal separator. Note that most currencies only accept up to 2 decimal places.
data ReceiptSummary = ReceiptSummary
    { rsSubtotal :: Maybe Double -- ^ Subtotal
    , rsShippingCost :: Maybe Double -- ^ Cost of shipping
    , rsTotalTax :: Maybe Double -- ^ Total tax
    , rsTotalCost :: Double -- ^ Total cost
    } deriving (Eq, Show, Read, Ord)

instance ToJSON ReceiptSummary where
  toJSON (ReceiptSummary subtotal shipcost ttax tcost) =
      object' [ "subtotal" .=!! subtotal
              , "shipping_cost" .=!! shipcost
              , "total_tax" .=!! ttax
              , "total_cost" .=! tcost
              ]

instance FromJSON ReceiptSummary where
  parseJSON = withObject "ReceiptSummary" $ \o ->
      ReceiptSummary <$> o .:? "subtotal"
                     <*> o .:? "shipping_cost"
                     <*> o .:? "total_tax"
                     <*> o .: "total_cost"

-- | Can be used to give discounts or other changes to the total
data ReceiptAdjustment = ReceiptAdjustment
    { raName :: Maybe Text -- Name of adjustment
    , raAmount :: Maybe Double -- Adjustment amount
    } deriving (Eq, Show, Read, Ord)

instance ToJSON ReceiptAdjustment where
  toJSON (ReceiptAdjustment name amount) =
      object' [ "name" .=!! name
              , "amount" .=!! amount
              ]

instance FromJSON ReceiptAdjustment where
  parseJSON = withObject "ReceiptAdjustment" $ \o ->
      ReceiptAdjustment <$> o .:? "name"
                        <*> o .:? "amount"
