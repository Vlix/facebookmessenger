{-# LANGUAGE TemplateHaskell #-}
module UnitTest.RequestParse.MessengerCode where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------------
-- MESSENGER CODE --
--------------------

messengerCodeTests :: TestTree
messengerCodeTests = Tasty.testGroup "Messenger Code Requests"
    [ messengerCodeTest
    , messengerCodeRef
    ]


messengerCodeVal :: Value
messengerCodeVal = $$(decodeFile "test/json/request/messenger_code_request.json")

messengerCodeTest :: TestTree
messengerCodeTest = parseTest "Messenger code" messengerCodeVal messengerCode

messengerCodeMaxVal :: Value
messengerCodeMaxVal = $$(decodeFile "test/json/request/messenger_code_request_ref.json")

messengerCodeRef :: TestTree
messengerCodeRef = parseTest "Maximal messenger code w/ parameter" messengerCodeMaxVal
                 $ MessengerCodeRequest (Just 1500) $ Just "billboard-ad"

