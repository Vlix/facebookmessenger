{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
import Web.Facebook.Messenger

import UnitTest.CallbackParse.AccountLinking (accountLinkTests)
import UnitTest.CallbackParse.AppRoleCallback (appRoleTests)
import UnitTest.CallbackParse.CheckoutUpdate (checkoutUpdateTest)
import UnitTest.CallbackParse.DeliveryCallback (deliveryTest)
import UnitTest.CallbackParse.EchoCallback (echoTests)
import UnitTest.CallbackParse.MessageCallback (messageTests)
import UnitTest.CallbackParse.PostbackCallback (postbackTests)
import UnitTest.CallbackParse.ReadCallback (readTest)
import UnitTest.CallbackParse.ReferralCallback (referralTests)
import UnitTest.CallbackParse.ThreadControl (threadControlTests)
import UnitTest.Internal


parseCallbackTests :: TestTree
parseCallbackTests = Tasty.testGroup "Parse Callbacks"
    [ callbackTests
    , accountLinkTests
    , appRoleTests
    , checkoutUpdateTest
    , deliveryTest
    , echoTests
    , messageTests
    , postbackTests
    , readTest
    , referralTests
    , threadControlTests
    ]

callbackTests :: TestTree
callbackTests = Tasty.testGroup "Callbacks"
    [ regularCallback
    , standByCallback
    , textCallbackPrior
    , customerMatchingAccepted
    ]


regularCallbackVal :: Value
regularCallbackVal = $$(decodeFile "test/json/callback/regular_text_callback.json")

standbyCallbackVal :: Value
standbyCallbackVal = $$(decodeFile "test/json/callback/standby_callback.json")

customerMatchingVal :: Value
customerMatchingVal = $$(decodeFile "test/json/callback/customer_matching_accepted.json")

regularCallback :: TestTree
regularCallback = testCase "Regular callback" $ sharedCallback False regularCallbackVal

standByCallback :: TestTree
standByCallback = testCase "Standby callback" $ sharedCallback True standbyCallbackVal

sharedCallback :: Bool -> Value -> Assertion
sharedCallback b val = eParse val @?= Right expected
  where expected = Callback [CallbackEntry pageID 1518439572110 msging b]
        msging = standardMessaging (Just 1518439590791)
                                   Nothing
                                   contnt
        contnt = CMMessage $ Message "mid.$cAAEIQkYwD5bNu1Kzh1hgg1r2bI_K"
                                     (Just 1370)
                                     msgContent
                                     []
        msgContent = MText $ MessageText "This is a text message" Nothing


textPrior :: Value
textPrior = $$(decodeFile "test/json/callback/text_callback_prior_message.json")

textCallbackPrior :: TestTree
textCallbackPrior = testCase "Text with PriorMessage" $
    eParse textPrior @?= Right expected
  where expected = standardMessaging (Just 1458692752478)
                                     (Just $ PriorMessage CheckBoxPlugin "<USER_REF>")
                                     contnt
        contnt = CMMessage $ Message "mid.1457744567618:41d102a3e1ae206a38"
                                     Nothing
                                     msgContent
                                     []
        msgContent = MText $ MessageText "Thanks for messaging me!" Nothing

customerMatchingAccepted :: TestTree
customerMatchingAccepted = testCase "Customer matching accepted" $
    eParse customerMatchingVal @?= Right expected
  where expected = standardMessaging (Just 1501755077175)
                                     Nothing
                                     CMMsgAccept
