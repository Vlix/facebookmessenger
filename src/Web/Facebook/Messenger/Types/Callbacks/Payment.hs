{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.Payment
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback occurs when the user taps the pay button from the checkout dialog produced via the `BuyButton`.
It contains the requested user information as well as payment credentials.
Depending on the payment provider you use, the payment credentials passed may differ.

You must subscribe to this callback by selecting the @"messaging_payments"@ field when setting up your webhook.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/payment
-}
module Web.Facebook.Messenger.Types.Callbacks.Payment (
  -- * Payment Callback
  Payment (..)
  , Amount (..)
  , RequestedUserInfo (..)
  -- ** Payment Credentials
  , PaymentCredential (..)
  , PaymentToken (..)
  , PaymentPayPal (..)
  , PaymentStripe (..)
  -- ** Other
  , DecryptedPaymentResult (..)
  )
where


import Control.Applicative ((<|>))
import Data.Aeson
import Data.Text

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests.Extra (TemplateAddress)


-- ------------------ --
--  PAYMENT CALLBACK  --
-- ------------------ --

-- | This callback occurs when the user taps the pay button from the checkout dialog produced via the `BuyButton`.
data Payment = Payment
    { pPayload :: Text -- ^ Metadata defined in the `BuyButton`.
    , pRequestedUserInfo :: RequestedUserInfo -- ^ Information that was requested from the user by the `BuyButton`.
    , pPaymentCredential :: PaymentCredential -- ^ Payment credentials.
    , pAmount :: Amount -- ^ Total amount of transaction.
    , pShippingOptionId :: Maybe Text
  -- ^ The `option_id` of the selected shipping option sent via the "CheckoutUpdate" callback. Only applicable for flexible payments.
    } deriving (Eq, Show, Read, Ord)

-- | Total amount of transaction.
data Amount = Amount
    { aCurrency :: Text -- ^ Currency of amount
    , aAmount :: Text -- ^ Total amount
    } deriving (Eq, Show, Read, Ord)

-- | Data in this object will depend on the requested user information defined on the `BuyButton`.
data RequestedUserInfo = RequestedUserInfo
    { ruiShippingAddress :: Maybe TemplateAddress -- ^ Person's shipping address
    , ruiContactName :: Maybe Text -- ^ Person's name
    , ruiContactEmail :: Maybe Text -- ^ Person's email address
    , ruiContactPhone :: Maybe Text -- ^ Person's phone number
    } deriving (Eq, Show, Read, Ord)

-- | Different payment provider types
data PaymentCredential = Token PaymentToken -- ^ Credit Card
                       | PayPal PaymentPayPal -- ^ PayPal
                       | Stripe PaymentStripe -- ^ Stripe
  deriving (Eq, Show, Read, Ord)

-- | Information about the credit card to use
data PaymentToken = PaymentToken
    { ptTokenizedCard :: Text -- ^ PGP-signed tokenized charge card
    , ptTokenizedCvv :: Text -- ^ PGP-signed CVV number
    , ptTokenExpiryMonth :: Text -- ^ Expiry month
    , ptTokenExpiryYear :: Text -- ^ Expiry year
    , ptFbPaymentId :: Text
    -- ^ A Facebook issued payment ID for tracking.
    -- (If it is a test payment, the id will be @"test_payment_id_12345"@.)
    } deriving (Eq, Show, Read, Ord)

-- | Information about the PayPal account used
data PaymentPayPal = PaymentPayPal
    { ppChargeId :: Text -- ^ PayPal charge id
    , ppFbPaymentId :: Text -- ^ A facebook issued payment id for tracking.
    } deriving (Eq, Show, Read, Ord)

-- | Information about the Stripe account used
data PaymentStripe = PaymentStripe
    { psChargeId :: Text -- ^ Stripe charge id
    , psFbPaymentId :: Text -- ^ A facebook issued payment id for tracking.
    } deriving (Eq, Show, Read, Ord)

-- | The result after decrypting a certain processed payment
--
-- https://developers.facebook.com/docs/messenger-platform/payments-reference#decrypting
data DecryptedPaymentResult = DecryptedPaymentResult
    { dprName :: Text -- ^ Person name
    , dprAmount :: Double -- ^ Amount of transaction
    , dprTimestamp :: Integer -- ^ Epoch timestamp
    , dprExternalTransactionId :: String
    -- ^ Transaction ID from payment processor
    -- (in case of test payment, the value will be @"test_charge_id_12345"@.)
    , dprFbPaymentId :: String
    -- ^ Facebook payment ID
    -- (in case of a test payment, the value will be @"test_payment_id_12345"@.)
    , dprProvider :: String -- ^ Facebook payment provider ID
    } deriving (Eq, Show, Read, Ord)


-- ---------------- --
--  JSON INSTANCES  --
-- ---------------- --

instance ToJSON Payment where
  toJSON (Payment payload rui pc amount shipid) =
      object' [ "payload"             .=! payload
              , "requested_user_info" .=! rui
              , "payment_credential"  .=! pc
              , "amount"              .=! amount
              , "shipping_option_id"  .=!! shipid
              ]

instance ToJSON RequestedUserInfo where
  toJSON (RequestedUserInfo address name email phone) =
      object' [ "shipping_address" .=!! address
              , "contact_name"     .=!! name
              , "contact_email"    .=!! email
              , "contact_phone"    .=!! phone
              ]

instance ToJSON PaymentCredential where
  toJSON (Token x) = toJSON x
  toJSON (PayPal x) = toJSON x
  toJSON (Stripe x) = toJSON x

instance ToJSON PaymentToken where
  toJSON (PaymentToken card cvv month year payid) =
      object [ "provider_type"      .= String "token"
             , "tokenized_card"     .= card
             , "tokenized_cvv"      .= cvv
             , "token_expiry_month" .= month
             , "token_expiry_year"  .= year
             , "fb_payment_id"      .= payid
             ]

instance ToJSON PaymentPayPal where
  toJSON (PaymentPayPal charge payid) =
      object [ "provider_type" .= String "paypal"
             , "charge_id"     .= charge
             , "fb_payment_id" .= payid
             ]

instance ToJSON PaymentStripe where
  toJSON (PaymentStripe charge payid) =
      object [ "provider_type" .= String "stripe"
             , "charge_id"     .= charge
             , "fb_payment_id" .= payid
             ]

instance ToJSON Amount where
  toJSON (Amount currency amount) =
      object [ "currency" .= currency
             , "amount"   .= amount
             ]

instance ToJSON DecryptedPaymentResult where
  toJSON (DecryptedPaymentResult name amount time extid payid provider) =
      object [ "name" .= name
             , "amount" .= amount
             , "timestamp" .= time
             , "external_transaction_id" .= extid
             , "fb_payment_id" .= payid
             , "provider" .= provider
             ]


instance FromJSON Payment where
  parseJSON = withObject "Payment" $ \o ->
      Payment <$> o .: "payload"
              <*> o .: "requested_user_info"
              <*> o .: "payment_credential"
              <*> o .: "amount"
              <*> o .:? "shipping_option_id"


instance FromJSON RequestedUserInfo where
  parseJSON = withObject "RequestedUserInfo" $ \o ->
      RequestedUserInfo <$> o .:? "shipping_address"
                        <*> o .:? "contact_name"
                        <*> o .:? "contact_email"
                        <*> o .:? "contact_phone"

instance FromJSON PaymentCredential where
  parseJSON = withObject "PaymentCredential" $ \o ->
        Token <$> parseJSON (Object o)
    <|> PayPal <$> parseJSON (Object o)
    <|> Stripe <$> parseJSON (Object o)

instance FromJSON PaymentToken where
  parseJSON = checkValue
      "PaymentToken"
      "provider_type"
      ("token" :: Text)
      $ \o -> PaymentToken <$> o .: "tokenized_card"
                           <*> o .: "tokenized_cvv"
                           <*> o .: "token_expiry_month"
                           <*> o .: "token_expiry_year"
                           <*> o .: "fb_payment_id"

instance FromJSON PaymentPayPal where
  parseJSON = checkValue
      "PaymentPayPal"
      "provider_type"
      ("paypal" :: Text)
      $ \o -> PaymentPayPal <$> o .: "charge_id"
                            <*> o .: "fb_payment_id"

instance FromJSON PaymentStripe where
  parseJSON = checkValue
      "PaymentStripe"
      "provider_type"
      ("stripe" :: Text)
      $ \o -> PaymentStripe <$> o .: "charge_id"
                            <*> o .: "fb_payment_id"

instance FromJSON Amount where
  parseJSON = withObject "Amount" $ \o ->
    Amount <$> o .: "currency"
           <*> o .: "amount"

instance FromJSON DecryptedPaymentResult where
  parseJSON = withObject "DecryptedPaymentResult" $ \o ->
      DecryptedPaymentResult <$> o .: "name"
                             <*> o .: "amount"
                             <*> o .: "timestamp"
                             <*> o .: "external_transaction_id"
                             <*> o .: "fb_payment_id"
                             <*> o .: "provider"
