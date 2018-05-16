{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.ReferralCallback where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

---------------
-- REFERRALS --
---------------

referralTests :: TestTree
referralTests = Tasty.testGroup "Referral Callbacks"
    [ referrals
    , referralCallback
    ]


referralsVal :: Value
referralsVal = $$(decodeFile "test/json/callback/referrals.json")

referralCBVal :: Value
referralCBVal = $$(decodeFile "test/json/callback/referral_callback.json")

referrals :: TestTree
referrals = parseTest "All referral types" referralsVal [ads, link, msngr, discover, plugin]
  where ads = ReferralAds $ RefAds (Just "optional_ref_data") "5346927245346"
        link = ReferralLink $ RefShortLink "some_cta_or_another"
        msngr = ReferralCode $ RefMessengerCode "some_ref_or_another"
        discover = ReferralDiscover
        plugin = ReferralChat $ RefChatPlugin (Just "<REF_DATA_PASSED_IN_CODE>")
                                              "https://some.website.com/chat/page.html"

referralCallback :: TestTree
referralCallback = parseTest "Referral callback" referralCBVal
                 $ standardMessaging (Just 1518442110333)
                                     Nothing
                                     contnt
  where contnt = CMReferral $ ReferralLink
                    $ RefShortLink "some_cta_or_another"
