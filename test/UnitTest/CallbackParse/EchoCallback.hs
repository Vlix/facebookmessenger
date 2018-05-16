{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.EchoCallback where


import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

----------
-- ECHO --
----------

echoTests :: TestTree
echoTests = Tasty.testGroup "Account Linking Callback"
    [ echoCallback
    , echoCallbackGeneric
    , echoCallbackButton
    , echoCallbackFallback
    ]


echoTextVal :: Value
echoTextVal = $$(decodeFile "test/json/callback/echo_callback_text.json")

echoGenericVal :: Value
echoGenericVal = $$(decodeFile "test/json/callback/echo_callback_generic.json")

echoButtonVal :: Value
echoButtonVal = $$(decodeFile "test/json/callback/echo_callback_button.json")

echoFallbackVal :: Value
echoFallbackVal = $$(decodeFile "test/json/callback/echo_callback_fallback.json")

echoCallback :: TestTree
echoCallback = parseTest "Echo callback" echoTextVal
             $ msg 1522851392983 $ CMEcho
                $ Echo True
                      (Just 743273262513025)
                       Nothing
                       "mid.$cAAFStIxHtgpFwwl111ikQQs_I_oI"
                      (Just 2860)
                      $ EText $ EchoText "test test"

echoCallbackGeneric :: TestTree
echoCallbackGeneric = parseTest "Echo callback (generic)" echoGenericVal
                    $ msg 1526045536891 $ CMEcho
                      $ Echo True
                             (Just 743273262513025)
                             Nothing
                             "mid.$cAAFSsZhFqTFpgKw6e1jT2bxuP4oa"
                             Nothing
                             $ EAttachment $ EchoAttachment [ea]
  where ea = genericTemplate_ (e1 :| [e2,e3])
        e1 = genericElem "<ELEMENT1>"
                         (Just "<SUBTITLE>")
                         (Just "http://some.url.com/somewhere.jpg/")
        e2 = genericElem "<ELEMENT2>"
                         Nothing
                         (Just "http://some.url.com/somewhere_else.jpg/")
        e3 = genericElem "<ELEMENT3>"
                         (Just "<SUBTITLE>")
                         Nothing

echoCallbackButton :: TestTree
echoCallbackButton = parseTest "Echo callback (button)" echoButtonVal
                   $ msg 1458696618268 $ CMEcho
                      $ Echo True
                          (Just 743273262513025)
                          (Just "<DEVELOPER_DEFINED_METADATA_STRING>")
                          "mid.1458696618141:b4ef9d19ec21086067"
                          Nothing
                          $ EButton $ EchoButton "Button text" $ btn :| []
  where btn = urlButton_ "Visit Messenger" "https://www.messenger.com/"

echoCallbackFallback :: TestTree
echoCallbackFallback = parseTest "Echo callback (fallback)" echoFallbackVal
                     $ msg 1525703917372 $ CMEcho
                        $ Echo True
                          Nothing
                          Nothing
                          "mid.$cAAFSsSdQ77ZpbE-HPajOwo1gvowP"
                          (Just 1716)
                          $ EFallback $ EchoFallback (Just "some optional text") [fb]
  where fb = Fallback (Just "<FALLBACK TITLE>")
                      (Just "<FALLBACK URL>")
                      Nothing

-- In the case of Echoes the sender and recipient can be switched,
-- so the Sender should actually be a PageID instead of a PSID,
-- but meh.
msg :: Integer -> CallbackContent -> CallbackMessaging
msg i = CallbackMessaging (Just $ CallbackSender $ PSID "234554838031189")
                          (CallbackRecipient $ PageID "1739141089378194")
                          (Just i)
                           Nothing
