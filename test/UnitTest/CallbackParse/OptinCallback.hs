{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.OptinCallback where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

-----------
-- OPTIN --
-----------

optinCallbackVal :: Value
optinCallbackVal = $$(decodeFile "test/json/callback/optin_callback.json")

optinTest :: TestTree
optinTest = parseTest "Optin Callback" optinCallbackVal
          $ standardMessaging (Just 1458692752478)
                              Nothing
                              $ CMOptin $ Optin "some_ref_or_another"
                                           $ Just "another_ref"
