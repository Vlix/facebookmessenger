{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.AppRoleCallback where


import Data.Aeson (Value)
import Data.HashMap.Strict (fromList)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

---------------
-- APP ROLES --
---------------

appRoleVal :: Value
appRoleVal = $$(decodeFile "test/json/callback/app_role_callback.json")

appRoleTests :: TestTree
appRoleTests = parseTest "App Role Callback" appRoleVal
             $ CallbackMessaging Nothing
                                 (CallbackRecipient $ PageID "234554838031189")
                                 (Just 1458692752478)
                                 Nothing
                                 contnt
  where contnt = CMAppRoles $ AppRoles $ fromList [("123456789",[PrimaryReceiver]),("098765432",[SecondaryReceiver])]
