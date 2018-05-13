{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.AppRoleCallback where


import Data.Aeson (Value)
import Data.HashMap.Strict (fromList)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------
-- POSTBACK --
--------------

appRoleVal :: Value
appRoleVal = $$(decodeFile "test/json/callback/app_role_callback.json")

appRoleTests :: TestTree
appRoleTests = Tasty.testCase "App Role Callback" $
    eParse appRoleVal @?= Right expected
  where expected = CallbackMessaging Nothing
                                     (CallbackRecipient $ PageID "234554838031189")
                                     (Just 1458692752478)
                                     Nothing
                                     contnt
        contnt = CMAppRoles $ AppRoles $ fromList [("123456789",[PrimaryReceiver]),("098765432",[SecondaryReceiver])]
