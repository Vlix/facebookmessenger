module Main where


import Test.Tasty as Tasty

import Aeson ( callbackTests
             , requestTests
             , responseTests
             , staticTests
             )
import UnitTest


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup
    "\nWeb.Facebook.Messenger"
        [ aesonTests
        , unitTests
        ]

aesonTests :: TestTree
aesonTests = Tasty.testGroup
    "Aeson"
        [ staticTests
        , requestTests
        , responseTests
        , callbackTests
        ]

unitTests :: TestTree
unitTests = Tasty.testGroup
    "Unit Tests"
        [ profileRequestTest
        , shortFunctionTests
        , parseCallbackTests
        , parseRequestTests
        , parseResponseTests
        ]
