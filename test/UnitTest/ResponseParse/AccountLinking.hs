{-# LANGUAGE TemplateHaskell #-}
module UnitTest.ResponseParse.AccountLinking where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

------------------
-- ACCOUNT LINK --
------------------

accountLinkingVal :: Value
accountLinkingVal = $$(decodeFile "test/json/response/account_linking.json")

accountLinkTest :: TestTree
accountLinkTest = parseTest "Account Linking Response" accountLinkingVal
                $ AccountLinkingResponse (PageID "234554838031189")
                  $ PSID "1218150762394951"
