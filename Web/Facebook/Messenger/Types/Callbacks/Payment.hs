{-# LANGUAGE RecordWildCards #-}

module Web.Facebook.Messenger.Types.Callbacks.Payment where


import Data.Aeson
import Data.Aeson.Types     (typeMismatch)
import Data.Text
import Data.HashMap.Strict  as HM

import Web.Facebook.Messenger.Types.Requests.Templates (TemplateAddress)
import Web.Facebook.Messenger.Types.Static


testPaymentCredentialToken :: PaymentCredential
testPaymentCredentialToken =
  PaymentCredentialToken "4111111111111111" "123" "11" "2020" "test_payment_id_12345"

testPaymentCredentialPayPalStripe :: PaymentCredential
testPaymentCredentialPayPalStripe =
  PaymentCredentialPayPal "test_charge_id_12345"  -- For a test_payment, the charge id will be test_charge_id_12345
                          "test_payment_id_12345" -- If it is a test payment, the id will be test_payment_id_12345.

-- ------------------ --
--  PAYMENT CALLBACK  --
-- ------------------ --

data Payment = Payment
  { payment_payload             :: Text              -- Metadata defined in the Buy Button.
  , payment_requested_user_info :: RequestedUserInfo -- Information that was requested from the user by the Buy Button.
  , payment_payment_credential  :: PaymentCredential -- Payment credentials.
  , payment_amount              :: Amount            -- Total amount of transaction.
  , payment_shipping_option_id  :: Text
-- The `option_id` of the selected shipping option sent via the checkout update callback. Only applicable for flexible payments.
  } deriving (Eq, Show)

data RequestedUserInfo = RequestedUserInfo
  { req_user_info_shipping_address :: Maybe TemplateAddress
  , req_user_info_contact_name     :: Maybe Text
  , req_user_info_contact_email    :: Maybe Text
  , req_user_info_contact_phone    :: Maybe Text
  } deriving (Eq, Show)

data PaymentCredential =
  PaymentCredentialToken
    { paycred_tokenized_card     :: Text -- PGP-signed tokenized charge card
    , paycred_tokenized_cvv      :: Text -- PGP-signed CVV number
    , paycred_token_expiry_month :: Text -- Expiry month
    , paycred_token_expiry_year  :: Text -- Expiry year
    , paycred_fb_payment_id      :: Text -- A facebook issued payment id for tracking.
    }
  | PaymentCredentialPayPal
    { paycred_charge_id     :: Text -- Payment provider charge id (for stripe/paypal)
    , paycred_fb_payment_id :: Text -- A facebook issued payment id for tracking.
    }
  | PaymentCredentialStripe
    { paycred_charge_id     :: Text -- Payment provider charge id (for stripe/paypal)
    , paycred_fb_payment_id :: Text -- A facebook issued payment id for tracking.
    } deriving (Eq, Show)

data Amount = Amount
  { amount_currency :: Text
  , amount_amount   :: Text
  } deriving (Eq, Show)

-- ---------------- --
--  JSON INSTANCES  --
-- ---------------- --

instance ToJSON Payment where
  toJSON Payment{..} =
    object [ "payload"             .= payment_payload
           , "requested_user_info" .= payment_requested_user_info
           , "payment_credential"  .= payment_payment_credential
           , "amount"              .= payment_amount
           , "shipping_option_id"  .= payment_shipping_option_id
           ]

instance ToJSON RequestedUserInfo where
  toJSON RequestedUserInfo{..} =
    object' [ "shipping_address" .=!! req_user_info_shipping_address
            , "contact_name"     .=!! req_user_info_contact_name
            , "contact_email"    .=!! req_user_info_contact_email
            , "contact_phone"    .=!! req_user_info_contact_phone
            ]

instance ToJSON PaymentCredential where
  toJSON PaymentCredentialToken{..} =
    object [ "provider_type"      .= String "token"
           , "tokenized_card"     .= paycred_tokenized_card
           , "tokenized_cvv"      .= paycred_tokenized_cvv
           , "token_expiry_month" .= paycred_token_expiry_month
           , "token_expiry_year"  .= paycred_token_expiry_year
           , "fb_payment_id"      .= paycred_fb_payment_id
           ]
  toJSON PaymentCredentialPayPal{..} =
    object [ "provider_type" .= String "paypal"
           , "charge_id"     .= paycred_charge_id
           , "fb_payment_id" .= paycred_fb_payment_id
           ]
  toJSON PaymentCredentialStripe{..} =
    object [ "provider_type" .= String "stripe"
           , "charge_id"     .= paycred_charge_id
           , "fb_payment_id" .= paycred_fb_payment_id
           ]

instance ToJSON Amount where
  toJSON (Amount currency amount) =
    object [ "currency" .= currency
           , "amount"   .= amount
           ]


instance FromJSON Payment where
  parseJSON = withObject "Payment" $ \o ->
    Payment <$> o .: "payload"
            <*> o .: "requested_user_info"
            <*> o .: "payment_credential"
            <*> o .: "amount"
            <*> o .: "shipping_option_id"


instance FromJSON RequestedUserInfo where
  parseJSON = withObject "RequestedUserInfo" $ \o ->
    RequestedUserInfo <$> o .:? "shipping_address"
                      <*> o .:? "contact_name"
                      <*> o .:? "contact_email"
                      <*> o .:? "contact_phone"

instance FromJSON PaymentCredential where
  parseJSON = withObject "PaymentCredential" $ \o ->
    case HM.lookup "provider_type" o of
      Just (String "token") ->
        PaymentCredentialToken <$> o .: "tokenized_card"
                               <*> o .: "tokenized_cvv"
                               <*> o .: "token_expiry_month"
                               <*> o .: "token_expiry_year"
                               <*> o .: "fb_payment_id"
      Just (String "paypal") ->
        PaymentCredentialPayPal <$> o .: "charge_id"
                                <*> o .: "fb_payment_id"
      Just (String "stripe") ->
        PaymentCredentialStripe <$> o .: "charge_id"
                                <*> o .: "fb_payment_id"
      Just wat -> fail $ "Unexpected value in provider_type key in PaymentCredential object: " `mappend` show wat
      _        -> fail "Expected provider_type key in PaymentCredential object"

instance FromJSON Amount where
  parseJSON = withObject "Amount" $ \o ->
    Amount <$> o .: "currency"
           <*> o .: "amount"
