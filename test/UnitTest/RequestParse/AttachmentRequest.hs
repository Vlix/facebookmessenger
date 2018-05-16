{-# LANGUAGE TemplateHaskell #-}
module UnitTest.RequestParse.AttachmentRequest where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

-------------------------
-- ATTACHMENT REQUESTS --
-------------------------

attachmentRequestTests :: TestTree
attachmentRequestTests = Tasty.testGroup "Attachment Requests"
    [ attachmentRequestTypes
    , attachmentRequestMedia
    , attachmentRequestMediaReusable
    ]


attachmentRequestMediaVal :: Value
attachmentRequestMediaVal = $$(decodeFile "test/json/request/attachment_request_media.json")

attachmentRequestMedia :: TestTree
attachmentRequestMedia = parseTest "Media attachment" attachmentRequestMediaVal
                       $ sendRequest (recipientID $ PSID "<PSID>")
                          $ attachmentRequest_ $ multimediaRequest_ VIDEO "http://www.example.com/video.mp4"

attachmentRequestMediaReusableVal :: Value
attachmentRequestMediaReusableVal = $$(decodeFile "test/json/request/attachment_request_media_reusable.json")

attachmentRequestMediaReusable :: TestTree
attachmentRequestMediaReusable = parseTest "Reusable media attachment" attachmentRequestMediaReusableVal
                               $ sendRequest (recipientID $ PSID "<PSID>")
                                  $ attachmentRequest_ $ multimediaRequest IMAGE "http://www.example.com/file.jpg" True

attachmentRequestTypesVal :: Value
attachmentRequestTypesVal = $$(decodeFile "test/json/request/attachment_request_media_types.json")

attachmentRequestTypes :: TestTree
attachmentRequestTypes = parseTest "Attachment types" attachmentRequestTypesVal $ [IMAGE, VIDEO, FILE, AUDIO]
