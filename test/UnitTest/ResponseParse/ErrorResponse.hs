{-# LANGUAGE TemplateHaskell #-}
module UnitTest.ResponseParse.ErrorResponse where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)
import Language.Haskell.TH (liftCode)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------------
-- ERROR RESPONSE --
--------------------

errorResponseTests :: TestTree
errorResponseTests = Tasty.testGroup "Error Response"
    [ errorResponseMinimal
    , errorResponseMaximal
    ]


errorResponseMinVal :: Value
errorResponseMinVal = $$(liftCode $ decodeFile "test/json/response/error_response_minimal.json")

errorResponseMaxVal :: Value
errorResponseMaxVal = $$(liftCode $ decodeFile "test/json/response/error_response_maximal.json")

errorResponseMinimal :: TestTree
errorResponseMinimal = parseTest "Minimal error response" errorResponseMinVal
                     $ ErrorResponse $ ErrorDetails
                                        { erMessage = "(#551) This person isn't available right now."
                                        , erType = "OAuthException"
                                        , erCode = 551
                                        , erErrorSubcode = Nothing
                                        , erFbtraceId = Nothing
                                        }

errorResponseMaximal :: TestTree
errorResponseMaximal = parseTest "Maximal error response" errorResponseMaxVal
                     $ ErrorResponse $ ErrorDetails
                                        { erMessage = "(#-1) Send API unexpected internal error"
                                        , erType = "OAuthException"
                                        , erCode = (-1)
                                        , erErrorSubcode = Just 2018012
                                        , erFbtraceId = Just "G1lQWWzZaIu"
                                        }
