{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.CheckoutUpdate where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

---------------------
-- CHECKOUT UPDATE --
---------------------

checkoutUpdateVal :: Value
checkoutUpdateVal = $$(decodeFile "test/json/callback/checkout_update.json")

checkoutUpdateTest :: TestTree
checkoutUpdateTest = parseTest "Checkout Update Callback" checkoutUpdateVal
                   $ standardMessaging (Just 1473204787206)
                                       Nothing
                                       contnt
  where contnt = CMCheckoutUpdate $ CheckoutUpdate "DEVELOPER_DEFINED_PAYLOAD"
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
