{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.DeliveryCallback where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------
-- DELIVERY --
--------------

deliveryCallbackVal :: Value
deliveryCallbackVal = $$(decodeFile "test/json/callback/delivery_callback.json")

deliveryTest :: TestTree
deliveryTest = parseTest "Delivery Callback" deliveryCallbackVal
             $ standardMessaging (Just 1488128564944)
                                 Nothing
                                 contnt
  where contnt = CMDelivery $ Delivery 1488128564613
                                       ["mid.1488128564673:5e67fce3a6"]
                                       $ Just 0
