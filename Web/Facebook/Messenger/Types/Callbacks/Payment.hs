module Web.Facebook.Messenger.Types.Callbacks.Payment where


import Data.Aeson
import Data.Text
import Data.HashMap.Strict as HM

import Web.Facebook.Messenger.Types.Requests.Templates (TemplateAddress)
import Web.Facebook.Messenger.Types.Static


-- ------------------ --
--  PAYMENT CALLBACK  --
-- ------------------ --

data Payment = Payment
    { pPayload :: Text -- Metadata defined in the Buy Button.
    , pRequestedUserInfo :: RequestedUserInfo -- Information that was requested from the user by the Buy Button.
    , pPaymentCredential :: PaymentCredential -- Payment credentials.
    , pAmount :: Amount -- Total amount of transaction.
    , pShippingOptionId :: Text
  -- The `option_id` of the selected shipping option sent via the checkout update callback. Only applicable for flexible payments.
    } deriving (Eq, Show)

data RequestedUserInfo = RequestedUserInfo
    { ruiShippingAddress :: Maybe TemplateAddress
    , ruiContactName :: Maybe Text
    , ruiContactEmail :: Maybe Text
    , ruiContactPhone :: Maybe Text
    } deriving (Eq, Show)

data PaymentCredential =
    PaymentCredentialToken
    { pcTokenizedCard :: Text -- PGP-signed tokenized charge card
    , pcTokenizedCvv :: Text -- PGP-signed CVV number
    , pcTokenExpiryMonth :: Text -- Expiry month
    , pcTokenExpiryYear :: Text -- Expiry year
    , pcFbPaymentId :: Text -- A facebook issued payment id for tracking.
    }
  | PaymentCredentialPayPal
    { pcChargeId :: Text -- Payment provider charge id (for stripe/paypal)
    , pcFbPaymentId :: Text -- A facebook issued payment id for tracking.
    }
  | PaymentCredentialStripe
    { pcChargeId :: Text -- Payment provider charge id (for stripe/paypal)
    , pcFbPaymentId :: Text -- A facebook issued payment id for tracking.
    } deriving (Eq, Show)

data Amount = Amount
    { aCurrency :: Text
    , aAmount :: Text
    } deriving (Eq, Show)

data DecryptedPaymentResult = DecryptedPaymentResult
    { dprName :: Text
    , dprAmount :: Amount
    , dprTimestamp :: Integer
    , dprExternalTransactionId :: String
    , dprFbPaymentId :: String
    , dprProvider :: String
    }


-- ---------------- --
--  JSON INSTANCES  --
-- ---------------- --

instance ToJSON Payment where
  toJSON (Payment payload rui pc amount shipid) =
      object [ "payload"             .= payload
             , "requested_user_info" .= rui
             , "payment_credential"  .= pc
             , "amount"              .= amount
             , "shipping_option_id"  .= shipid
             ]

instance ToJSON RequestedUserInfo where
  toJSON (RequestedUserInfo address name email phone) =
      object' [ "shipping_address" .=!! address
              , "contact_name"     .=!! name
              , "contact_email"    .=!! email
              , "contact_phone"    .=!! phone
              ]

instance ToJSON PaymentCredential where
  toJSON (PaymentCredentialToken card cvv month year payid) =
      object [ "provider_type"      .= String "token"
             , "tokenized_card"     .= card
             , "tokenized_cvv"      .= cvv
             , "token_expiry_month" .= month
             , "token_expiry_year"  .= year
             , "fb_payment_id"      .= payid
             ]
  toJSON (PaymentCredentialPayPal charge payid) =
      object [ "provider_type" .= String "paypal"
             , "charge_id"     .= charge
             , "fb_payment_id" .= payid
             ]
  toJSON (PaymentCredentialStripe charge payid) =
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

instance FromJSON DecryptedPaymentResult where
  parseJSON = withObject "DecryptedPaymentResult" $ \o ->
      DecryptedPaymentResult <$> o .: "name"
                             <*> o .: "amount"
                             <*> o .: "timestamp"
                             <*> o .: "external_transaction_id"
                             <*> o .: "fb_payment_id"
                             <*> o .: "provider"
