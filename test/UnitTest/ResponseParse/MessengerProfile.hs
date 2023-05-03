{-# LANGUAGE TemplateHaskell #-}
module UnitTest.ResponseParse.MessengerProfile where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)
import Language.Haskell.TH (liftCode)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

-----------------------
-- MESSENGER PROFILE --
-----------------------

messengerProfileVal :: Value
messengerProfileVal = $$(liftCode $ decodeFile "test/json/response/messenger_profile_response.json")

messengerProfileTest :: TestTree
messengerProfileTest = parseTest "Messenger Profile Response" messengerProfileVal
                     $ GetProfileResponse obj
  where obj = greeting [ defaultGreeting "Hello!"
                       , GreetingSetting (Just FBen_US) "Timeless apparel for the masses."
                       ]
           <> whiteListedDomains ["https://facebook.com/"]
           <> targetAudience (TargetAudience CUSTOM $ Just $ TargetCountries [] ["US", "NL", "JP"])
           <> persistentMenu [ PersistentMenuSetting Nothing False
                                [ persistentPostbackItem "<TITLE>"       "<PAYLOAD>"
                                , persistentPostbackItem "<OTHER_TITLE>" "<OTHER_PAYLOAD>"
                                ]
                                False
                             ]
