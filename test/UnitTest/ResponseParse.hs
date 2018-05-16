module UnitTest.ResponseParse where


import Test.Tasty as Tasty

import UnitTest.ResponseParse.AccountLinking (accountLinkTest)
import UnitTest.ResponseParse.AttachmentUpload (attachmentUploadTest)
import UnitTest.ResponseParse.BroadcastResponse (broadcastTests)
import UnitTest.ResponseParse.ErrorResponse (errorResponseTests)
import UnitTest.ResponseParse.MessageResponse (messageResponseTests)
import UnitTest.ResponseParse.MessengerCode (messengerCodeTest)
import UnitTest.ResponseParse.MessengerProfile (messengerProfileTest)
import UnitTest.ResponseParse.ThreadControl (threadControlTests)
import UnitTest.ResponseParse.TagResponse (tagResponseTest)
import UnitTest.ResponseParse.SuccessResponse (successResponseTest)
import UnitTest.ResponseParse.UserProfileResponse (userProfileResponseTest)


parseResponseTests :: TestTree
parseResponseTests = Tasty.testGroup "Parse Responses"
    [ accountLinkTest
    , attachmentUploadTest
    , broadcastTests
    , errorResponseTests
    , messageResponseTests
    , messengerCodeTest
    , messengerProfileTest
    , threadControlTests
    , tagResponseTest
    , successResponseTest
    , userProfileResponseTest
    ]
