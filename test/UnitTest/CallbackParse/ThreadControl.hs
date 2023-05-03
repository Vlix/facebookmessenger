{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.ThreadControl where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)
import Language.Haskell.TH (liftCode)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------------
-- THREAD CONTROL --
--------------------

threadControlTests :: TestTree
threadControlTests = Tasty.testGroup "Thread Control Callbacks"
    [ passThreadCallback
    , requestThreadCallback
    , takeThreadCallback
    ]


passThreadVal :: Value
passThreadVal = $$(liftCode $ decodeFile "test/json/callback/pass_thread_control.json")

requestThreadVal :: Value
requestThreadVal = $$(liftCode $ decodeFile "test/json/callback/request_thread_control.json")

takeThreadVal :: Value
takeThreadVal = $$(liftCode $ decodeFile "test/json/callback/take_thread_control.json")

passThreadCallback :: TestTree
passThreadCallback = parseTest "Pass thread" passThreadVal
                   $ msg $ CMPassThread $ PassThread (AppId "123456789")
                            $ Just "Additional content that the caller wants to set"

requestThreadCallback :: TestTree
requestThreadCallback = parseTest "Request thread" requestThreadVal
                      $ msg $ CMRequestThread $ RequestThread (AppId "123456789")
                              $ Just "additional content that the caller wants to set"

takeThreadCallback :: TestTree
takeThreadCallback = parseTest "Take thread" takeThreadVal
                   $ msg $ CMTakeThread $ TakeThread (AppId "123456789")
                            $ Just "additional content that the caller wants to set!"

msg :: CallbackContent -> CallbackMessaging
msg contnt = standardMessaging (Just 1458692752478)
                               Nothing
                               contnt
