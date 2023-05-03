{-# LANGUAGE TemplateHaskell #-}
module UnitTest.RequestParse.TemplateRequests where


import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Yaml.TH (decodeFile)
import Language.Haskell.TH (liftCode)

import Test.Tasty as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

-----------------------
-- TEMPLATE REQUESTS --
-----------------------

templateRequestTests :: TestTree
templateRequestTests = Tasty.testGroup "Template Requests"
    [ templateButtons
    , genericTemplateMinimal
    , genericTemplateMaximal
    , listTemplateMinimal
    , listTemplateMaximal
    ]


templateButtonsVal :: Value
templateButtonsVal = $$(liftCode $ decodeFile "test/json/request/template_buttons.json")

templateButtons :: TestTree
templateButtons = parseTest "Template buttons" templateButtonsVal
                  [ logoutButton
                  , loginButton "<YOUR_LOGIN_URL>"
                  , callButton "<BUTTON_TEXT>" "<PHONE_NUMBER>"
                  , buyButton "<STRING_SENT_TO_WEBHOOK>"
                              "EUR"
                              FIXED_AMOUNT
                              "<YOUR_BUSINESS_NAME>"
                              [SHIPPING_ADDRESS,CONTACT_NAME,CONTACT_PHONE,CONTACT_EMAIL]
                              [PriceObject "<ITEM_NAME>" "<ITEM_PRICE>"]
                  , shareButton $ Just $ genericElem "just some title" Nothing Nothing
                  , shareButton Nothing
                  , postbackButton "<BUTTON_TEXT>" "<STRING_SENT_TO_WEBHOOK>"
                  , urlButton_ "<BUTTON_TEXT>" "<URL_TO_OPEN_IN_WEBVIEW>"
                  ]

genericTemplateMinVal :: Value
genericTemplateMinVal = $$(liftCode $ decodeFile "test/json/request/template_request_generic_minimal.json")

genericTemplateMinimal :: TestTree
genericTemplateMinimal = parseTest "Minimal generic template" genericTemplateMinVal
                       $ sendRequest (recipientID $ PSID "1254444444682919")
                                     $ attachmentRequest_ $ genericTemplate_
                                        $ genericElem "Some element or another" Nothing Nothing :| []

genericTemplateMaxVal :: Value
genericTemplateMaxVal = $$(liftCode $ decodeFile "test/json/request/template_request_generic.json")

genericTemplateMaximal :: TestTree
genericTemplateMaximal = parseTest "Maximal generic template" genericTemplateMaxVal
                       $ sendRequest (recipientID $ PSID "1254444444682919")
                                     $ attachmentRequest qrs Nothing templ
  where templ = genericTemplate_ $ firstElem :| [secondElem]
        firstElem = GenericElement
                      { geTitle = "CatapultsR'Us Flights"
                      , geSubtitle = Just "€80,00"
                      , geImageUrl = Just "<SOME_URL>"
                      , geDefaultAction = Nothing
                      , geBuyButton = Nothing
                      , geButtons = [ shareButton Nothing
                                    , urlButton_ "Meer info" "http://example.ofa.flight/flight/8321307457"
                                    ]
                      }
        secondElem = GenericElement
                      { geTitle = "<TITLE_TEXT>"
                      , geSubtitle = Just "<SUBTITLE_TEXT>"
                      , geImageUrl = Just "<IMAGE_URL_TO_DISPLAY>"
                      , geDefaultAction = Just $ defaultAction "<DEFAULT_URL_TO_OPEN>"
                      , geBuyButton = Nothing
                      , geButtons = [ postbackButton "<BUTTON_TEXT>" "<STRING_SENT_TO_WEBHOOK>"
                                    , urlButton_ "<BUTTON_TEXT>" "<URL_TO_OPEN_IN_WEBVIEW>"
                                    ]
                      }
        qrs = [ qr_ "😍 Onthoud deze!" "remember"
              , qr_ "👎 Vergeet deze!" "forget"
              ]

listTemplateMinVal :: Value
listTemplateMinVal = $$(liftCode $ decodeFile "test/json/request/template_request_list_minimal.json")

listTemplateMinimal :: TestTree
listTemplateMinimal = parseTest "Minimal list template" listTemplateMinVal
                    $ sendRequest (recipientID $ PSID "1254444444682919")
                                  $ attachmentRequest_ $ listTemplate_
                                     $ listElem "First item" Nothing Nothing :|
                                       [ listElem "Second item"
                                                  (Just "Blablabla")
                                                  (Just "https://www.example.com/media/images/1024x576-pretty-picture.jpg")
                                       ]

listTemplateMaxVal :: Value
listTemplateMaxVal = $$(liftCode $ decodeFile "test/json/request/template_request_list_maximal.json")

listTemplateMaximal :: TestTree
listTemplateMaximal = parseTest "Maximal list template" listTemplateMaxVal
                    $ sendRequest (recipientID $ PSID "RECIPIENT_ID") $ attachmentRequest_ templ
  where templ = listTemplate ListCOMPACT es $ Just topLvlBtn
        es = firstElem :| [secondElem, thirdElem]
        topLvlBtn = urlButton_ "Other info" "https://somewhere.else.com/"
        firstElem = ListElement
                      { leTitle = "Classic T-Shirt Collection"
                      , leSubtitle = Just "See all our colors"
                      , leImageUrl = Just "https://peterssendreceiveapp.ngrok.io/img/collection.png"
                      , leDefaultAction = Nothing
                      , leButton = Just $ urlButtonME "View"
                                                      ( "https://peterssendreceiveapp.ngrok.io/collection"
                                                      , "https://peterssendreceiveapp.ngrok.io/"
                                                      )
                                                      TALL
                                                      SHOW
                      }
        secondElem = ListElement
                       { leTitle = "Classic White T-Shirt"
                       , leSubtitle = Just "See all our colors"
                       , leImageUrl = Nothing
                       , leDefaultAction = Just $ defaultAction "https://peterssendreceiveapp.ngrok.io/view?item=100"
                       , leButton = Nothing
                       }
        thirdElem = ListElement
                      { leTitle = "Classic Blue T-Shirt"
                      , leSubtitle = Just "100% Cotton, 200% Comfortable"
                      , leImageUrl = Just "https://peterssendreceiveapp.ngrok.io/img/blue-t-shirt.png"
                      , leDefaultAction = Just $ defaultActionME
                                    ( "https://peterssendreceiveapp.ngrok.io/view?item=101"
                                    , "https://peterssendreceiveapp.ngrok.io/"
                                    )
                                    TALL
                                    SHOW
                      , leButton = Just $ urlButtonME "Shop Now"
                                            ( "https://peterssendreceiveapp.ngrok.io/shop?item=101"
                                            , "https://peterssendreceiveapp.ngrok.io/"
                                            )
                                            TALL
                                            SHOW
                      }