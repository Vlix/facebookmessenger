module UnitTest.Internal where


import Data.Aeson (FromJSON(..), Value)
import Data.Aeson.Types (parseEither)

import Web.Facebook.Messenger


pageID :: PageID
pageID = PageID "234554838031189"

standardRecipient :: CallbackRecipient
standardRecipient = CallbackRecipient pageID

psid :: PSID
psid = PSID "1739141089378194"

standardSender :: Maybe CallbackSender
standardSender = Just $ CallbackSender psid

standardMessaging :: Maybe Integer -> Maybe PriorMessage -> CallbackContent -> CallbackMessaging
standardMessaging timestmp priormsg contnt =
  CallbackMessaging standardSender
                    standardRecipient
                    timestmp
                    priormsg
                    contnt

eParse :: FromJSON a => Value -> Either String a
eParse = parseEither parseJSON
