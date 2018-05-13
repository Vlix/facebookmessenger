module UnitTest.ProfileRequest (
  profileRequestTest
  ) where


import Data.Semigroup

import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
import Web.Facebook.Messenger

import Instances()

profileRequestTest :: TestTree
profileRequestTest = Tasty.testGroup "Profile Request"
    [ testEmpty
    , testGreetings
    , testGetStarted
    , testPersistentMenu
    , testPersistentUrlDefault
    , testWhiteListed
    , testAccountLinking
    , testPaymentSettings
    , testTargetAudience
    , testHomeUrl
    , testHomeUrlDefault
    , testMonoidRule
    ]

testEmpty :: TestTree
testEmpty = testCase "Empty profile request stays empty" $
    (mempty :: ProfileRequest) <> mempty @?= mempty

testGreetings :: TestTree
testGreetings = testCase "Greetings concatinate lists" $
    greeting [defGreet, nlGreet] <>
    greeting [enGreet] @?= greeting [defGreet, nlGreet, enGreet]
  where defGreet = defaultGreeting "Hello, test"
        nlGreet = GreetingSetting "nl_NL" "Hallo, test"
        enGreet = GreetingSetting "en_US" "Hello, testing"

testGetStarted :: TestTree
testGetStarted = testCase "Get started takes last" $
    getStarted "first" <> secondGetStarted
        @?= secondGetStarted
  where secondGetStarted = getStarted "second"

testPersistentMenu :: TestTree
testPersistentMenu = testCase "Persistent menu concatinates lists" $
    persistentMenu [firstSetting] <>
    persistentMenu [secondSetting] <>
    persistentMenu [thirdSetting] @?= finalPersistentMenu
  where firstSetting = PersistentMenuSetting Nothing False
          [persistentUrlItem_ "test" "https://test.net/"]
          False
        secondSetting = PersistentMenuSetting (Just "nl_NL") True
          [persistentPostbackItem "click" "somemetadatahere"]
          True
        thirdSetting = PersistentMenuSetting Nothing True
          [persistentNestedItem "testNested"
            [persistentUrlItem "test2" "http://bleh.us/somewhere" COMPACT False Nothing HIDE
            ,persistentPostbackItem "click2" "somemoremetadatahere"
            ]
          ]
          False
        finalPersistentMenu = persistentMenu
          [firstSetting, secondSetting, thirdSetting]

testPersistentUrlDefault :: TestTree
testPersistentUrlDefault = testCase "persistentUrlItem_ uses correct defaults" $
    persistentUrlItem_ title url @?=
      PMIUrl (URLButton title url FULL False Nothing SHOW)
  where title = "test"
        url = "http://test.url.org/"

testWhiteListed :: TestTree
testWhiteListed = testCase "Whitelisted domains concatinate as well" $
    whiteListedDomains ["http://example.one.com/"] <>
    whiteListedDomains ["https://second.example.net/", "https://alternative.second.example.net/"]
      @?= whiteListedDomains [ "http://example.one.com/"
                             , "https://second.example.net/"
                             , "https://alternative.second.example.net/"
                             ]

testAccountLinking :: TestTree
testAccountLinking = testCase "Account linking url takes last" $
    accountLinkingUrl "http://trying.something.old/" <>
    accountLinkingUrl "http://trying.something.new/"
      @?= accountLinkingUrl "http://trying.something.new/"

testPaymentSettings :: TestTree
testPaymentSettings = testCase "Payment settings also takes last" $
    paymentSettings (PaymentSettings (Just "http://no.one.cares/") (Just "somekey") ["347895203067030853","2905601723075464"]) <>
    paymentSettings (PaymentSettings Nothing (Just "somekey") ["236368473684133693","895764312023546657"])
      @?= paymentSettings (PaymentSettings Nothing (Just "somekey") ["236368473684133693","895764312023546657"])

testTargetAudience :: TestTree
testTargetAudience = testCase "Target audience also takes last" $
    targetAudience (TargetAudience ALL Nothing) <>
    targetAudience (TargetAudience CUSTOM (Just $ TargetCountries [] ["EN", "NL", "DE"]))
      @?= targetAudience (TargetAudience CUSTOM (Just $ TargetCountries [] ["EN", "NL", "DE"]))

testHomeUrl :: TestTree
testHomeUrl = testCase "Home URL also takes last" $
    homeUrl (HomeUrl "https://my.home.url/" SHOW False) <>
    homeUrl (HomeUrl "https://my.actual.home/" HIDE True) <>
    finalUrl @?= finalUrl
  where finalUrl = homeUrl (HomeUrl "https://just-kidding.thisis.home/" SHOW True)

testHomeUrlDefault :: TestTree
testHomeUrlDefault = testCase "persistentUrlItem_ uses correct defaults" $
    homeUrl_ url @?= homeDef
  where url = "http://test.url.org/"
        homeDef = homeUrl (HomeUrl url HIDE False)

testMonoidRule :: TestTree
testMonoidRule = testCase "Profile request should stay the same if appended with mempty" $
    mempty <> original <> mempty @?= original
  where original = greeting [defaultGreeting "Hello, test"]
                <> getStarted "testing"
                <> persistentMenu [PersistentMenuSetting Nothing True
                                    [persistentNestedItem "testNested"
                                      [persistentUrlItem "test2" "http://bleh.us/somewhere" COMPACT False Nothing HIDE
                                      ,persistentPostbackItem "click2" "somemoremetadatahere"
                                      ]
                                    ]
                                    False
                                  ]
                <> whiteListedDomains ["http://example.one.com/"]
                <> accountLinkingUrl "http://trying.something.new/"
                <> homeUrl (HomeUrl "https://my.home.url/" SHOW False)
                <> targetAudience (TargetAudience CUSTOM (Just $ TargetCountries [] ["EN", "NL", "DE"]))
