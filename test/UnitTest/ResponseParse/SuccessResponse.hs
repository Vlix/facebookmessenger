{-# LANGUAGE TemplateHaskell #-}
module UnitTest.ResponseParse.SuccessResponse where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)
import Language.Haskell.TH (liftCode)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

----------------------
-- SUCCESS RESPONSE --
----------------------

successResponseVal :: Value
successResponseVal = $$(liftCode $ decodeFile "test/json/response/success_response.json")

successResponseTest :: TestTree
successResponseTest = parseTest "Success Response" successResponseVal
                    $ SuccessResponse "unlink account success"
