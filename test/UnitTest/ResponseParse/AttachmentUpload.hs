{-# LANGUAGE TemplateHaskell #-}
module UnitTest.ResponseParse.AttachmentUpload where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

----------------
-- ATTACHMENT --
----------------

attachmentUploadVal :: Value
attachmentUploadVal = $$(decodeFile "test/json/response/attachment_upload.json")

attachmentUploadTest :: TestTree
attachmentUploadTest = parseTest "Attachment Upload Response" attachmentUploadVal
                     $ AttachmentUploadResponse "687799999980546"
