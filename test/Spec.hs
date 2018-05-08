{-# LANGUAGE OverloadedStrings #-}
module Main where


import Test.Tasty as Tasty

import Aeson ( callbackTests
             , requestTests
             , responseTests
             , staticTests
             )
import Functions (profileRequestTest)


main :: IO ()
main = Tasty.defaultMain
     $ Tasty.testGroup "\nWeb.Facebook.Messenger"
        [ aesonTests
        , functionTests
        -- , unitTests
        ]

aesonTests :: TestTree
aesonTests = Tasty.testGroup "Aeson"
    [ staticTests
    , requestTests
    , responseTests
    , callbackTests
    ]

functionTests :: TestTree
functionTests = Tasty.testGroup "Helper Functions"
    [ profileRequestTest
    ]
