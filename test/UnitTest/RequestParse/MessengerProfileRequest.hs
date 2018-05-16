{-# LANGUAGE TemplateHaskell #-}
module UnitTest.RequestParse.MessengerProfileRequest where


import Data.Aeson (Value)
import Data.Semigroup ((<>))
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

-----------------------
-- MESSENGER PROFILE --
-----------------------

messengerProfileVal :: Value
messengerProfileVal = $$(decodeFile "test/json/request/messenger_profile_request.json")

messengerProfileTest :: TestTree
messengerProfileTest = parseTest "Messenger Profile Request" messengerProfileVal
                     $ greeting [defaultGreeting "Hello!"]
                    <> greeting [GreetingSetting "en_US" "Timeless apparel for the masses."]
                    <> whiteListedDomains ["https://www.something.com/"]
                    <> targetAudience (TargetAudience CUSTOM $ Just $ TargetCountries ["US","NL","JP"] [])
                    <> persistentMenu [PersistentMenuSetting Nothing False items False]
  where items = [ persistentUrlItem_ "<TITLE>" "<SOME_URL>"
                , persistentPostbackItem "<OTHER_TITLE>" "<PAYLOAD>"
                , persistentNestedItem "<NESTED_TITLE>"
                    [persistentPostbackItem "Contact Info" "CONTACT_INFO_PAYLOAD"]
                ]
