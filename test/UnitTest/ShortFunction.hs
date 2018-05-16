module UnitTest.ShortFunction (
  shortFunctionTests
  ) where


import Data.List.NonEmpty
import Data.Text

import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
import Web.Facebook.Messenger

import Instances()

shortFunctionTests :: TestTree
shortFunctionTests = Tasty.testGroup "Default functions"
    [ testQR
    , testURLButton
    , testGenElem
    , testGenericTemp
    , testGeneric
    , testListElem
    , testListTemp
    , testList
    , testDefaultAction
    , testMultimediaRequest
    , testMultimediaRequest_
    , testAttachmentRequest
    , testTextRequest
    , testSendRequest
    , testSendRequestTag
    , testMessengerCode
    ]

testQR :: TestTree
testQR = testCase "QR without image icon" $
    qr_ txt payload @?= RQR (RQuickReply txt payload Nothing)
  where txt = "testing"
        payload = "testPayload"

testGenElem :: TestTree
testGenElem = testCase "Basic generic element" $
    genericElem txt subTitle mUrl @?=
        GenericElement txt subTitle mUrl Nothing Nothing []
  where txt = "title"
        subTitle = Just "subTitle"
        mUrl = Just "someUrl"

genTestElem :: Text -> NonEmpty GenericElement
genTestElem txt = genericElem txt Nothing Nothing :| []

genTestTemp :: Text -> GenericTemplate
genTestTemp = GenericTemplate True HORIZONTAL . genTestElem

testGenericTemp :: TestTree
testGenericTemp = testCase "Generic template payload" $
    genericTemplateP_ (genTestElem txt) @?= expected
  where txt = "some test"
        expected = TGeneric $ genTestTemp txt

testGeneric :: TestTree
testGeneric = testCase "Generic Template default" $
    genericTemplate_ (genTestElem txt) @?= expected
  where txt = "testing"
        expected = RTemplate $ RequestAttachmentTemplate
                 $ TGeneric $ genTestTemp txt

testURLButton :: TestTree
testURLButton = testCase "URL button with only title and url" $
    urlButton_ title url @?= TUrl (URLButton title url FULL False Nothing SHOW)
  where title = "testing"
        url = "https://at.some.site/some/path?or=something"

testDefaultAction :: TestTree
testDefaultAction = testCase "Default action default" $
    defaultAction url @?= expected
  where url = "https://somewhere.or.not/"
        expected = DefaultAction url FULL False Nothing SHOW

testListElem :: TestTree
testListElem = testCase "Basic list element" $
    listElem txt subTitle mUrl @?=
        ListElement txt subTitle mUrl Nothing Nothing
  where txt = "title2"
        subTitle = Just "subTitle2"
        mUrl = Just "someUrl2"

listTestElem :: Text -> NonEmpty ListElement
listTestElem txt = listElem txt Nothing Nothing :| []

listTestTemp :: Text -> ListTemplate
listTestTemp txt = ListTemplate ListLARGE (listTestElem txt) Nothing

testListTemp :: TestTree
testListTemp = testCase "List template payload" $
    listTemplateP_ (listTestElem txt) @?= expected
  where txt = "some test2"
        expected = TList $ listTestTemp txt

testList :: TestTree
testList = testCase "List Template default" $
    listTemplate_ (listTestElem txt) @?= expected
  where txt = "testing2"
        expected = RTemplate $ RequestAttachmentTemplate
                 $ TList $ listTestTemp txt

testMultimediaRequest :: TestTree
testMultimediaRequest = testCase "Multimedia attachment" $
    multimediaRequest typ url False @?= expected
  where url = "https://some.where.com/image.jpg"
        typ = IMAGE
        expected = testMultimediaAtt typ url False

testMultimediaRequest_ :: TestTree
testMultimediaRequest_ = testCase "Multimedia non-reusable attachment" $
    multimediaRequest_ typ url @?= expected
  where url = "https://some.where.com/image.jpg"
        typ = IMAGE
        expected = testMultimediaAtt typ url False

testMultimediaAtt :: AttachmentType -> URL -> Bool -> RequestAttachment
testMultimediaAtt typ url reuse =
    RMultimedia $ RequestMultimediaAttachment typ
                $ RMPayload $ RMultimediaPayload url reuse

testAttachmentRequest :: TestTree
testAttachmentRequest = testCase "Attachment request without QR or metadata" $
    attachmentRequest_ att @?= expected
  where expected = RMAttachment $ RequestMessageAttachment att [] Nothing
        url = "https://idunno.maybe.here/some/image.jpg"
        att = testMultimediaAtt IMAGE url False

testTextRequest :: TestTree
testTextRequest = testCase "Text request without QR or metadata" $
    textRequest_ txt @?= expected
  where txt = "testing"
        expected = RMText $ RequestMessageText txt [] Nothing

testSendRequest :: TestTree
testSendRequest = testCase "Default send request as RESPONSE, REGULAR notification, no tag" $
    sendRequest recp reqMsg @?= expected
  where reqMsg = textRequest_ "this is a message"
        recp = recipientID $ PSID "38478935789035536083701"
        expected = SendRequest recp reqMsg RESPONSE REGULAR Nothing

testSendRequestTag :: TestTree
testSendRequestTag = testCase "Send request with tag for outside 24+1 window" $
    sendRequestTag recp reqMsg tag @?= expected
  where reqMsg = textRequest_ "this is a message"
        recp = recipientID $ PSID "38478935789035536083701"
        tag = APPOINTMENT_UPDATE
        expected = SendRequest recp reqMsg MESSAGE_TAG REGULAR $ Just tag

testMessengerCode :: TestTree
testMessengerCode = testCase "Messenger Code request with no values" $
    messengerCode @?= MessengerCodeRequest Nothing Nothing
