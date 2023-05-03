{-# LANGUAGE TemplateHaskell #-}
module UnitTest.RequestParse.ThreadControl where


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
threadControlTests = Tasty.testGroup "Thread Control Requests"
    [ passThreadControl
    , threadControlTest
    ]


passThreadControlVal :: Value
passThreadControlVal = $$(liftCode $ decodeFile "test/json/request/thread_control_pass.json")

passThreadControl :: TestTree
passThreadControl = parseTest "Pass thread" passThreadControlVal
                  $ PassThreadControlRequest (RecipientID $ PSID "<PSID>")
                                             (AppId "123456789")
                                             $ Just "String to pass to secondary receiver app"

threadControlVal :: Value
threadControlVal = $$(liftCode $ decodeFile "test/json/request/thread_control_take-request.json")

threadControlTest :: TestTree
threadControlTest = parseTest "Thread control (take/request)" threadControlVal
                  $ ThreadControlRequest (RecipientID $ PSID "<PSID>")
                      $ Just "String to pass to primary receiver app"

