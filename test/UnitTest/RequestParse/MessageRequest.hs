{-# LANGUAGE TemplateHaskell #-}
module UnitTest.RequestParse.MessageRequest where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

---------------------
-- MESSAGE REQUEST --
---------------------

messageRequestTests :: TestTree
messageRequestTests = Tasty.testGroup "Message Requests"
    [ recipientTest
    , senderActionTypes
    , senderActionTest
    , textRequestMinimal
    , textRequestMaximal
    , textRequestQRs
    ]


recipientVal :: Value
recipientVal = $$(decodeFile "test/json/request/recipient.json")

recipientTest :: TestTree
recipientTest = parseTest "Recipient types" recipientVal
              $ [ recipientRef "<UNIQUE_REF_PARAM>"
                , recipientID $ PSID "<PAGE_ID>"
                , recipientPhone "<PHONE_NUMBER>" Nothing
                , recipientPhone "<PHONE_NUMBER2>" $ Just
                    $ RecipientName "<FIRST_NAME>" "<LAST_NAME>"
                ]

senderActionVal :: Value
senderActionVal = $$(decodeFile "test/json/request/sender_action_request.json")

senderActionTest :: TestTree
senderActionTest = parseTest "Sender action request" senderActionVal
               $ SenderActionRequest (recipientID $ PSID "<PSID>") TYPING_ON

senderActionsVal :: Value
senderActionsVal = $$(decodeFile "test/json/request/sender_actions.json")

senderActionTypes :: TestTree
senderActionTypes = parseTest "Message request w/ attachment id" senderActionsVal
                  $ [TYPING_ON, TYPING_OFF, MARK_SEEN]

textRequestMinVal :: Value
textRequestMinVal = $$(decodeFile "test/json/request/text_request_minimal.json")

textRequestMinimal :: TestTree
textRequestMinimal = parseTest "Minimal text request" textRequestMinVal
                   $ SendRequest (recipientID $ PSID "<PSID>")
                                 (textRequest_ "<TEXT_MESSAGE>")
                                 UPDATE
                                 REGULAR
                                 Nothing

textRequestMaxVal :: Value
textRequestMaxVal = $$(decodeFile "test/json/request/text_request_maximal.json")

textRequestMaximal :: TestTree
textRequestMaximal = parseTest "Maximal text request" textRequestMaxVal
                   $ SendRequest (recipientID $ PSID "<PSID>")
                                 (textRequest [] (Just "<SOME_METADATA>") "<SOME_TEXT>")
                                 MESSAGE_TAG
                                 SILENT_PUSH
                                 (Just SHIPPING_UPDATE)

textRequestQRVal :: Value
textRequestQRVal = $$(decodeFile "test/json/request/text_request_qrs.json")

textRequestQRs :: TestTree
textRequestQRs = parseTest "Text request w/ QRs" textRequestQRVal
                   $ sendRequest (recipientID $ PSID "<PSID>")
                                 (textRequest qrs Nothing "Here is a quick reply!")
  where qrs = [ qr "Search" "<POSTBACK_PAYLOAD>" $ Just "http://example.com/img/red.png"
              , qr_ "Press here" "<POSTBACK_PAYLOAD2>"
              , locQR
              , phoneQR
              , emailQR
              ]
