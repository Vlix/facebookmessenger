{-# LANGUAGE TemplateHaskell #-}
module UnitTest.ResponseParse.MessageResponse where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)
import Language.Haskell.TH (liftCode)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

----------------------
-- MESSAGE RESPONSE --
----------------------

messageResponseTests :: TestTree
messageResponseTests = Tasty.testGroup "Message Response"
    [ messageResponseRegular
    , messageResponseReferral
    , messageResponseAttachment
    ]


messageResponseVal :: Value
messageResponseVal = $$(liftCode $ decodeFile "test/json/response/message_response_regular.json")

messageResponseRefVal :: Value
messageResponseRefVal = $$(liftCode $ decodeFile "test/json/response/message_response_ref.json")

messageResponseAttVal :: Value
messageResponseAttVal = $$(liftCode $ decodeFile "test/json/response/message_response_attachment.json")

messageResponseRegular :: TestTree
messageResponseRegular = parseTest "Regular message response" messageResponseVal
                       $ MessageResponse (Just $ PSID "1254444444682919")
                                         "mid.$cAAJsujCd2ORkHh27-ld7NhzuqrUK"
                                         Nothing

messageResponseReferral :: TestTree
messageResponseReferral = parseTest "Message response w/o PSID" messageResponseRefVal
                        $ MessageResponse Nothing
                                          "mid.$cAAJsujCd2ORkHh27-ld7NhzuqrUK"
                                          Nothing

messageResponseAttachment :: TestTree
messageResponseAttachment = parseTest "Message response w/ attachment id" messageResponseAttVal
                          $ MessageResponse (Just $ PSID "1254444444682919")
                                            "mid.$cAAJsujCd2ORkHh27-ld7NhzuqrUK"
                                            (Just "687799999980546")
