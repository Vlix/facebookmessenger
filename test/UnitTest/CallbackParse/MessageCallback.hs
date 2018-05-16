{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.MessageCallback where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------
-- MESSAGES --
--------------

messageTests :: TestTree
messageTests = Tasty.testGroup "Message Callbacks"
    [ textCallbackQR
    , attachmentImage
    , attachmentAudio
    , attachmentVideo
    , attachmentFallback
    , attachmentTemplate
    , attachmentLocation
    , attachmentSticker
    ]


textQRVal :: Value
textQRVal = $$(decodeFile "test/json/callback/text_qr_callback.json")

imageVal :: Value
imageVal = $$(decodeFile "test/json/callback/attachment_callback_image.json")

videoVal :: Value
videoVal = $$(decodeFile "test/json/callback/attachment_callback_video.json")

audioVal :: Value
audioVal = $$(decodeFile "test/json/callback/attachment_callback_audio.json")

fallbackVal :: Value
fallbackVal = $$(decodeFile "test/json/callback/attachment_callback_fallback.json")

templateVal :: Value
templateVal = $$(decodeFile "test/json/callback/attachment_callback_template.json")

locationVal :: Value
locationVal = $$(decodeFile "test/json/callback/attachment_callback_location.json")

stickerVal :: Value
stickerVal = $$(decodeFile "test/json/callback/attachment_callback_sticker.json")

textCallbackQR :: TestTree
textCallbackQR = parseTest "Text with QR" textQRVal
               $ standardMessaging (Just 1485785260154)
                                   Nothing
                                   contnt
  where contnt = CMMessage $ Message "mid.1483715260354:77ac33da15"
                                     (Just 8668)
                                     msgContent
                                     []
        msgContent = MText $ MessageText "CLICK HERE" (Just $ CallbackQuickReply "some_payload")

attachmentImage :: TestTree
attachmentImage = parseTest "Image attachment" imageVal
                $ standardMessaging (Just 1521927088604)
                                    Nothing
                                    contnt
  where contnt = CMMessage $ Message "mid.$cAAFSuiOlE3doibGxAFiWxoQ_dosX"
                                     (Just 9583)
                                     msgContent
                                     []
        msgContent = MAttachment $ MessageAttachment [att]
        att = CAMultimedia $ MultimediaAttachment IMAGE
                           $ CallbackMultimediaPayload "https://scontent-ort2-2.xx.fbcdn.net/v/t34.0-12/29138552_211240296290531_291369741_n.jpg?_nc_cat=0&_nc_ad=z-m&_nc_cid=0&oh=65d9fcb6baefd7481f5dc477a76571c3&oe=5AB85055"

attachmentAudio :: TestTree
attachmentAudio = parseTest "Audio attachment" audioVal
                $ standardMessaging (Just 1522751139155)
                                    Nothing
                                    contnt
  where contnt = CMMessage $ Message "mid.$cAAFSsSTn2XdcvE-1U1i5wpqbrOHw"
                                     (Just 3808)
                                     msgContent
                                     []
        msgContent = MAttachment $ MessageAttachment [att]
        att = CAMultimedia $ MultimediaAttachment AUDIO
                           $ CallbackMultimediaPayload "https://cdn.fbsbx.com/v/t59.3654-21/29451513_1977941472233841_8301729723659059200_n.aac/audioclip-1522751138417-2894.aac?_nc_cat=0&oh=a395e53726c98b7e0d60a5d30edcbeab&oe=5AC60884"

attachmentVideo :: TestTree
attachmentVideo = parseTest "Video attachment" videoVal
                $ standardMessaging (Just 1524463431537)
                                    Nothing
                                    contnt
  where contnt = CMMessage $ Message "mid.$cAAFSt2aYrFJpI8x3ci8RnJfhPTsQ"
                                     (Just 2869)
                                     msgContent
                                     []
        msgContent = MAttachment $ MessageAttachment [att]
        att = CAMultimedia $ MultimediaAttachment VIDEO
                           $ CallbackMultimediaPayload "https://video-ort2-2.xx.fbcdn.net/v/t42.3356-2/31126804_1758684374210244_6397927484334269799_n.mp4/video-1524463431.mp4?_nc_cat=0&vabr=88639&oh=b5ccd0dbf1547bbf3b1c9fcaf4007cd3&oe=5ADF020B"

attachmentFallback :: TestTree
attachmentFallback = parseTest "Fallback attachment" fallbackVal
                   $ standardMessaging (Just 1484828179384)
                                       Nothing
                                       contnt
  where contnt = CMMessage $ Message "mid.1484828179384:02c39cc697"
                                     (Just 5322)
                                     msgContent
                                     []
        msgContent = MAttachment $ MessageAttachment [att]
        att = CAFallback $ Fallback (Just "Check us out!")
                                    (Just "https://www.facebook.com/Thisisus/")
                                    Nothing

attachmentTemplate :: TestTree
attachmentTemplate = parseTest "Template attachment" templateVal
                   $ standardMessaging (Just 1497895209689)
                                       Nothing
                                       contnt
  where contnt = CMMessage $ Message "mid.$cAAD2N5dYXFdi8shS2VcwYMKB82oI"
                                     (Just 126384)
                                     msgContent
                                     []
        msgContent = MAttachment $ MessageAttachment [att]
        att = CATemplate $ TemplateAttachment (Just "<SOME_TITLE>")
                                              Nothing
                                              (Just "https://www.example.com/")
                                              $ CallbackTemplate (Just True) [e]
        e = GenericElement "<ELEMENT_TITLE>"
                           (Just " ")
                           (Just "https://www.example.com/some_image.jpg")
                           (Just $ DefaultAction "http://www.example.com/" FULL False Nothing SHOW)
                           Nothing
                           []

attachmentLocation :: TestTree
attachmentLocation = parseTest "Location attachment" locationVal
                   $ standardMessaging (Just 1519133473383)
                                       Nothing
                                       contnt
  where contnt = CMMessage $ Message "mid.$cAAFSsb24gKln5K6Sb1hshk46ukmJ"
                                     (Just 115715)
                                     msgContent
                                     []
        msgContent = MLocation $ MessageLocation [att]
        att = CallbackLocation (Just "Someone's Location")
                               (Just "<SOME URL USING BING MAPS>")
                               $ CallbackLocationPayload
                                 $ CallbackCoordinates 52.376232 4.838296

attachmentSticker :: TestTree
attachmentSticker = parseTest "Sticker attachment" stickerVal
                  $ standardMessaging (Just 1486142376165)
                                      Nothing
                                      contnt
  where contnt = CMMessage $ Message "mid.1482142576415:5aaf79ce93"
                                     (Just 31674)
                                     msgContent
                                     []
        msgContent = MSticker $ MessageSticker [att] 369239263222822
        att = StickerAttachment
                $ CallbackStickerPayload "https://scontent.xx.fbcdn.net/v/t39.1997-6/851557_369239266556155_759568595_n.png?_nc_ad=z-m&oh=6db01d9d3eb168d058cbb2d6692af58f&oe=58978D5C"
                                         369239263222822
