{-# LANGUAGE TemplateHaskell #-}
module UnitTest.RequestParse.AccountUnlinking where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

-----------------------
-- ACCOUNT UNLINKING --
-----------------------

accountUnlinkVal :: Value
accountUnlinkVal = $$(decodeFile "test/json/request/account_unlinking.json")

accountUnlinkTest :: TestTree
accountUnlinkTest = parseTest "Account Unlinking Request" accountUnlinkVal
                  $ AccountUnlinkRequest $ PSID "PSID"
