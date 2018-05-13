{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.DeliveryCallback where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------
-- POSTBACK --
--------------

deliveryCallbackVal :: Value
deliveryCallbackVal = $$(decodeFile "test/json/callback/delivery_callback.json")

deliveryTest :: TestTree
deliveryTest = Tasty.testCase "Delivery Callback" $
    eParse deliveryCallbackVal @?= Right expected
  where expected = standardMessaging (Just 1488128564944)
                               Nothing
                               contnt
        contnt = CMDelivery $ Delivery 1488128564613
                                       ["mid.1488128564673:5e67fce3a6"]
                                       $ Just 0
