{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.ReadCallback where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

----------
-- READ --
----------

readCallbackVal :: Value
readCallbackVal = $$(decodeFile "test/json/callback/read_callback.json")

readTest :: TestTree
readTest = testCase "Read Callback" $
    eParse readCallbackVal @?= Right expected
  where expected = standardMessaging (Just 1518439572105)
                               Nothing
                               contnt
        contnt = CMRead $ ReadCallback 1518439571400 $ Just 0
