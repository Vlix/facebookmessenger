{-# LANGUAGE TemplateHaskell #-}
module UnitTest.ResponseParse.MessengerCode where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------------
-- MESSENGER CODE --
--------------------

messengerCodeVal :: Value
messengerCodeVal = $$(decodeFile "test/json/response/messenger_code.json")

messengerCodeTest :: TestTree
messengerCodeTest = parseTest "Messenger Code Response" messengerCodeVal
                $ MessengerCodeResponse "<YOUR_CODE_URL_HERE>"
