{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.ReadCallback where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

----------
-- READ --
----------

readCallbackVal :: Value
readCallbackVal = $$(decodeFile "test/json/callback/read_callback.json")

readTest :: TestTree
readTest = parseTest "Read Callback" readCallbackVal
         $ standardMessaging (Just 1518439572105)
                             Nothing
                             contnt
  where contnt = CMRead $ ReadCallback 1518439571400 $ Just 0
