{-# LANGUAGE TemplateHaskell #-}
module UnitTest.ResponseParse.UserProfileResponse where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)
import Language.Haskell.TH (liftCode)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

---------------------------
-- USER PROFILE RESPONSE --
---------------------------

userProfileResponseVal :: Value
userProfileResponseVal = $$(liftCode $ decodeFile "test/json/response/user_profile.json")

userProfileResponseTest :: TestTree
userProfileResponseTest = parseTest "User Profile Response" userProfileResponseVal
                        $ UserProfileResponse
                            { uprId = "1286146702283499"
                            , uprFirstName = Just "John"
                            , uprLastName = Just "Smith"
                            , uprProfilePic = Just "https://lookaside.facebook.com/platform/profilepic/?psid=1286146702283499&width=1024&ext=1526314033&hash=AeW3QpAe7PtwX5fM"
                            , uprLocale = Just "en_GB"
                            , uprTimezone = Just 2
                            , uprGender = Just "male"
                            , uprIsPaymentEnabled = Just True
                            , uprLastAdReferral = Just "6045246247433"
                            , uprEmail = Nothing
                            }
