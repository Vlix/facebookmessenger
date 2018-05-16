module UnitTest.RequestParse where


import Test.Tasty as Tasty

import UnitTest.RequestParse.AccountUnlinking (accountUnlinkTest)
import UnitTest.RequestParse.AttachmentRequest (attachmentRequestTests)
import UnitTest.RequestParse.MessageRequest (messageRequestTests)
import UnitTest.RequestParse.MessengerCode (messengerCodeTest)
import UnitTest.RequestParse.MessengerProfileRequest (messengerProfileTest)
import UnitTest.RequestParse.TemplateRequests (templateRequestTests)
import UnitTest.RequestParse.ThreadControl (threadControlTests)


parseRequestTests :: TestTree
parseRequestTests = Tasty.testGroup "Parse Requests"
    [ accountUnlinkTest
    , attachmentRequestTests
    , messageRequestTests
    , messengerCodeTest
    , messengerProfileTest
    , templateRequestTests
    , threadControlTests
    ]
