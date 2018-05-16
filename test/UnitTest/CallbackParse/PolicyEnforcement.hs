{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.PolicyEnforcement where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

------------------------
-- POLICY ENFORCEMENT --
------------------------

policyTests :: TestTree
policyTests = Tasty.testGroup "Policy Enforcement Callback"
    [ policyTypes
    , policyCallback
    ]

policyCallbackVal :: Value
policyCallbackVal = $$(decodeFile "test/json/callback/policy_enforcement.json")

policyTypesVal :: Value
policyTypesVal = $$(decodeFile "test/json/callback/policy_enforcement_types.json")

policyCallback :: TestTree
policyCallback = parseTest "Policy enforcement callback" policyCallbackVal
               $ CallbackMessaging Nothing
                                   (CallbackRecipient $ PageID "234554838031189")
                                   (Just 1458692752478)
                                   Nothing
                                   contnt
  where contnt = CMPolicy $ Block "The bot violated our Platform Policies"

policyTypes :: TestTree
policyTypes = parseTest "Policy enforcement types" policyTypesVal
            $ [Block "The bot violated our Platform Policies", Unblock]
