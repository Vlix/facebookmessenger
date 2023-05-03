{-# LANGUAGE TemplateHaskell #-}
module UnitTest.ResponseParse.BroadcastResponse where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)
import Language.Haskell.TH (liftCode)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

-------------------------
-- BROADCAST RESPONSES --
-------------------------

broadcastTests :: TestTree
broadcastTests = Tasty.testGroup "Broadcast Message Reponse"
    [ messageCreativeTest
    , broadcastMessageTest
    ]

messageCreativeVal :: Value
messageCreativeVal = $$(liftCode $ decodeFile "test/json/response/message_creative_response.json")

broadcastMessageVal :: Value
broadcastMessageVal = $$(liftCode $ decodeFile "test/json/response/broadcast_message_response.json")

messageCreativeTest :: TestTree
messageCreativeTest = parseTest "Message creative response" messageCreativeVal
                    $ MessageCreativeResponse 938461089

broadcastMessageTest :: TestTree
broadcastMessageTest = parseTest "Broadcast message response" broadcastMessageVal
                     $ BroadcastMessageResponse 827
