{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.AccountLinking where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)
import Language.Haskell.TH (liftCode)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

------------------
-- ACCOUNT LINK --
------------------

accountLinkTests :: TestTree
accountLinkTests = Tasty.testGroup "Account Linking Callback"
    [ accountLinkCallback
    , accountLinks
    ]


accountLinkingVal :: Value
accountLinkingVal = $$(liftCode $ decodeFile "test/json/callback/account_linking_callback.json")

accountLinksVal :: Value
accountLinksVal = $$(liftCode $ decodeFile "test/json/callback/account_linking_types.json")

accountLinkCallback :: TestTree
accountLinkCallback = parseTest "Account link callback" accountLinkingVal
                    $ standardMessaging (Just 1234567890)
                                        Nothing
                                        contnt
  where contnt = CMAccountLink $ AccountLink $ Just "PASS_THROUGH_AUTHORIZATION_CODE"

accountLinks :: TestTree
accountLinks = parseTest "Account linking types" accountLinksVal
             $ [ AccountLink $ Just "PASS_THROUGH_AUTHORIZATION_CODE"
               , AccountUnlink
               ]
