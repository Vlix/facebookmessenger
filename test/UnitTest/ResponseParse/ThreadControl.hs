{-# LANGUAGE TemplateHaskell #-}
module UnitTest.ResponseParse.ThreadControl where


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
threadControlTests = Tasty.testGroup "Thread Control Responses"
    [ threadControl
    , secondaryReceivers
    , threadOwnerResponse
    ]


threadControlVal :: Value
threadControlVal = $$(liftCode $ decodeFile "test/json/response/thread_control.json")

threadControl :: TestTree
threadControl = parseTest "Success response" threadControlVal
              $ ThreadControlResponse True


secondaryReceiversVal :: Value
secondaryReceiversVal = $$(liftCode $ decodeFile "test/json/response/secondary_receiver.json")

secondaryReceivers :: TestTree
secondaryReceivers = parseTest "Request secondary receivers" secondaryReceiversVal
                   $ DataResponse [ SecondaryReceiverElement (Just $ AppId "12345678910") (Just "David's Composer")
                                  , SecondaryReceiverElement (Just $ AppId "23456789101") (Just "Messenger Rocks")
                                  ]

threadOwnerVal :: Value
threadOwnerVal = $$(liftCode $ decodeFile "test/json/response/thread_owner_response.json")

threadOwnerResponse :: TestTree
threadOwnerResponse = parseTest "Thread owner response" threadOwnerVal
                    $ ThreadOwnerResponse $ AppId "1517776481860111"
