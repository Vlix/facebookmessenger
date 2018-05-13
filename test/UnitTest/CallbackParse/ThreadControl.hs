{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.ThreadControl where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
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
passThreadVal = $$(decodeFile "test/json/callback/pass_thread_control.json")

requestThreadVal :: Value
requestThreadVal = $$(decodeFile "test/json/callback/request_thread_control.json")

takeThreadVal :: Value
takeThreadVal = $$(decodeFile "test/json/callback/take_thread_control.json")

passThreadCallback :: TestTree
passThreadCallback = testCase "Pass thread" $
    eParse passThreadVal @?= Right expected
  where expected = msg $ CMPassThread $ PassThread (AppId "123456789")
                          $ Just "Additional content that the caller wants to set"

requestThreadCallback :: TestTree
requestThreadCallback = testCase "Request thread" $
    eParse requestThreadVal @?= Right expected
  where expected = msg $ CMRequestThread $ RequestThread (AppId "123456789")
                          $ Just "additional content that the caller wants to set"

takeThreadCallback :: TestTree
takeThreadCallback = testCase "Take thread" $
    eParse takeThreadVal @?= Right expected
  where expected = msg $ CMTakeThread $ TakeThread (AppId "123456789")
                          $ Just "additional content that the caller wants to set!"

msg :: CallbackContent -> CallbackMessaging
msg contnt = standardMessaging (Just 1458692752478)
                               Nothing
                               contnt
