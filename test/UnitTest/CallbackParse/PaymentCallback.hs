{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.PaymentCallback where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------
-- PAYMENTS --
--------------

paymentTests :: TestTree
paymentTests = Tasty.testGroup "Payment Callback"
    [ paymentTypes
    , callbackTest
    ]


paymentCallbackVal :: Value
paymentCallbackVal = $$(decodeFile "test/json/callback/payment_callback.json")

paymentCredentialsVal :: Value
paymentCredentialsVal = $$(decodeFile "test/json/callback/payment_credential_types.json")

callbackTest :: TestTree
callbackTest = parseTest "Payment callback" paymentCallbackVal
             $ standardMessaging (Just 1473208792799)
                                 Nothing
                                 contnt
  where contnt = CMPayment $ Payment "DEVELOPER_DEFINED_PAYLOAD"
                                     reqUserInfo
                                     creds
                                     (Amount "USD" "29.62")
                                     $ Just "123"
        reqUserInfo = RequestedUserInfo address
                                        (Just "Peter Chang")
                                        (Just "peter@anemailprovider.com")
                                        (Just "+15105551234")
        address = Just $ TemplateAddress
            { taStreet1 = "1 Hacker Way"
            , taStreet2 = Just ""
            , taCity = "MENLO PARK"
            , taPostalCode = "94025"
            , taState = "CA"
            , taCountry = "US"
            }
        creds = Stripe $ PaymentStripe "ch_18tmdBEoNIH3FPJHa60ep123" "123456789"

paymentTypes :: TestTree
paymentTypes = parseTest "Credential types" paymentCredentialsVal [stripe, paypal, token]
  where stripe = Stripe $ PaymentStripe "ch_18tmdBEoNIH3FPJHa60ep123" "123456789"
        paypal = PayPal $ PaymentPayPal "test_charge_id_12345" "9078563412"
        token = Token $ PaymentToken
                          { ptTokenizedCard = "__tokenized_card__"
                          , ptTokenizedCvv = "tokenized cvv"
                          , ptTokenExpiryMonth = "3"
                          , ptTokenExpiryYear = "2019"
                          , ptFbPaymentId = "0987654321"
                          }

