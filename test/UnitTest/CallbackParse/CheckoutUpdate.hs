{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.CheckoutUpdate where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------
-- POSTBACK --
--------------

checkoutUpdateVal :: Value
checkoutUpdateVal = $$(decodeFile "test/json/callback/checkout_update.json")

checkoutUpdateTest :: TestTree
checkoutUpdateTest = Tasty.testCase "Checkout Update Callback" $
    eParse checkoutUpdateVal @?= Right expected
  where expected = standardMessaging (Just 1473204787206)
                               Nothing
                               contnt
        contnt = CMCheckoutUpdate $ CheckoutUpdate "DEVELOPER_DEFINED_PAYLOAD"
                                                   "10105655000959552"
                                                   shipAddress
        shipAddress = TemplateAddress
            { taStreet1 = "1 Hacker Way"
            , taStreet2 = Just ""
            , taCity = "MENLO PARK"
            , taPostalCode = "94025"
            , taState = "CA"
            , taCountry = "US"
            }
