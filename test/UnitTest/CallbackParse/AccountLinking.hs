{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.AccountLinking where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------
-- POSTBACK --
--------------

accountLinkTests :: TestTree
accountLinkTests = Tasty.testGroup "Account Linking Callback"
    [ accountLinkCallback
    , accountLinks
    ]


accountLinkingVal :: Value
accountLinkingVal = $$(decodeFile "test/json/callback/account_linking_callback.json")

accountLinksVal :: Value
accountLinksVal = $$(decodeFile "test/json/callback/account_linking_types.json")

accountLinkCallback :: TestTree
accountLinkCallback = testCase "Account link callback" $
    eParse accountLinkingVal @?= Right expected
  where expected = standardMessaging (Just 1234567890)
                               Nothing
                               contnt
        contnt = CMAccountLink $ AccountLink $ Just "PASS_THROUGH_AUTHORIZATION_CODE"

accountLinks :: TestTree
accountLinks = testCase "Account linking types" $
    eParse accountLinksVal @?= Right expected
  where expected = [ AccountLink $ Just "PASS_THROUGH_AUTHORIZATION_CODE"
                   , AccountUnlink
                   ]
