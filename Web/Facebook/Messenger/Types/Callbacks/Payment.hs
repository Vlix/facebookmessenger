module Web.Facebook.Messenger.Types.Callbacks.Payment where


import Control.Applicative  ((<|>))
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
  toJSON (Payment payload rui paycred amount shipoptid) =
    object [ "payload"             .= payload
           , "requested_user_info" .= rui
           , "payment_credential"  .= paycred
           , "amount"              .= amount
           , "shipping_option_id"  .= shipoptid
           ]

instance ToJSON RequestedUserInfo where
  toJSON (RequestedUserInfo shipping_address contact_name contact_email contact_phone) =
    object' [ "shipping_address" .=!! shipping_address
            , "contact_name"     .=!! contact_name
            , "contact_email"    .=!! contact_email
            , "contact_phone"    .=!! contact_phone
            ]

instance ToJSON PaymentCredential where
  toJSON (PaymentCredentialToken card cvv expmonth expyear fbpayid) =
    object [ "provider_type"      .= String "token"
           , "tokenized_card"     .= card
           , "tokenized_cvv"      .= cvv
           , "token_expiry_month" .= expmonth
           , "token_expiry_year"  .= expyear
           , "fb_payment_id"      .= fbpayid
           ]
  toJSON (PaymentCredentialPayPal charge_id fbpayid) =
    object [ "provider_type" .= String "paypal"
           , "charge_id"     .= charge_id
           , "fb_payment_id" .= fbpayid
           ]
  toJSON (PaymentCredentialStripe charge_id fbpayid) =
    object [ "provider_type" .= String "stripe"
           , "charge_id"     .= charge_id
           , "fb_payment_id" .= fbpayid
           ]

instance ToJSON Amount where
  toJSON (Amount currency amount) =
    object [ "currency" .= currency
           , "amount"   .= amount
           ]


instance FromJSON Payment where
  parseJSON (Object o) =
    Payment <$> o .: "payload"
            <*> o .: "requested_user_info"
            <*> o .: "payment_credential"
            <*> o .: "amount"
            <*> o .: "shipping_option_id"
  parseJSON wat = typeMismatch "Payment" wat

instance FromJSON RequestedUserInfo where
  parseJSON (Object o) =
    RequestedUserInfo <$> o .:? "shipping_address"
                      <*> o .:? "contact_name"
                      <*> o .:? "contact_email"
                      <*> o .:? "contact_phone"
  parseJSON wat = typeMismatch "RequestedUserInfo" wat

instance FromJSON PaymentCredential where
  parseJSON (Object o) = case HM.lookup "provider_type" o of
    Just (String "token")  -> PaymentCredentialToken <$> o .: "tokenized_card"
                                            <*> o .: "tokenized_cvv"
                                            <*> o .: "token_expiry_month"
                                            <*> o .: "token_expiry_year"
                                            <*> o .: "fb_payment_id"
    Just (String "paypal") -> PaymentCredentialPayPal <$> o .: "charge_id"
                                             <*> o .: "fb_payment_id"
    Just (String "stripe") -> PaymentCredentialStripe <$> o .: "charge_id"
                                             <*> o .: "fb_payment_id"
    Just wat -> fail $ "Unexpected value in provider_type key in PaymentCredential object: " `mappend` show wat
    _        -> fail "Expected provider_type key in PaymentCredential object"
  parseJSON wat = typeMismatch "PaymentCredential" wat

instance FromJSON Amount where
  parseJSON (Object o) = Amount <$> o .: "currency"
                                <*> o .: "amount"
  parseJSON wat = typeMismatch "Amount" wat
