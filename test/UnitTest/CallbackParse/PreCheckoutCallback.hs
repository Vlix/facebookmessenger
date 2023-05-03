{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.PreCheckoutCallback where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)
import Language.Haskell.TH (liftCode)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

----------
-- READ --
----------

preCheckoutCallbackVal :: Value
preCheckoutCallbackVal = $$(liftCode $ decodeFile "test/json/callback/pre_checkout_callback.json")

preCheckoutTest :: TestTree
preCheckoutTest = parseTest "Pre Checkout Callback" preCheckoutCallbackVal
                $ standardMessaging (Just 1473208792799)
                                    Nothing
                                    contnt
  where contnt = CMPreCheckout $ PreCheckout "xyz" reqUserInfo $ Amount "USD" "2.70"
        reqUserInfo = RequestedUserInfo address
                                        (Just "Tao Jiang")
                                        Nothing
                                        Nothing
        address = Just $ TemplateAddress
            { taStreet1 = "600 Edgewater Blvd"
            , taStreet2 = Just ""
            , taCity = "Foster City"
            , taPostalCode = "94404"
            , taState = "CA"
            , taCountry = "US"
            }
