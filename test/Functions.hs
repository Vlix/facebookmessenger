{-# LANGUAGE OverloadedStrings #-}
module Functions where


import Test.Tasty as Tasty
-- import Web.Facebook.Messenger

import Instances()

profileRequestTest :: TestTree
profileRequestTest = Tasty.testGroup "Profile Request" []
    -- [ test1 ]
{-
test1 :: TestTree
test1 = testCase "Empty profile request stays empty" undefined
-}