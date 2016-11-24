module Web.Facebook.Messenger
    ( handleMessaging
    , senderAction
    , messageText
    , attachmentImage
    , attachmentAudio
    , attachmentVideo
    , attachmentFile
    , genericTemplate
    , buttonTemplate
    , listTemplate
    , module Web.Facebook.Messenger.Types
    -- All functions with prime ' use the RecipientPhone number instead of its PSID (Page-Scoped ID)
    , senderAction'
    , messageText'
    , attachmentImage'
    , attachmentAudio'
    , attachmentVideo'
    , attachmentFile'
    , genericTemplate'
    , buttonTemplate'
    , listTemplate'
    ) where

import Data.Text

import Web.Facebook.Messenger.Types



handleMessaging :: CallbackHandlers a -> CallbackMessaging -> a
handleMessaging fbcbh (CallbackMessagingMessage (CallbackSender sident)
                                                (CallbackRecipient rident)
                                                time
                                                (CallbackMessageText mid message quickreply seq')
                      ) = messageHandler fbcbh sident rident time mid message quickreply seq'
                
handleMessaging fbcbh (CallbackMessagingMessage (CallbackSender sident)
                                                (CallbackRecipient rident)
                                                time
                                                (CallbackMessageAttachment mid attachments seq')
                      ) = attachmentHandler fbcbh sident rident time mid attachments seq'

handleMessaging fbcbh (CallbackMessagingMessage (CallbackSender sident)
                                                (CallbackRecipient rident)
                                                time
                                                (CallbackMessageLocation mid locations seq')
                      ) = locationHandler fbcbh sident rident time mid locations seq'
handleMessaging fbcbh (CallbackMessagingAuth (CallbackSender sident)
                                             (CallbackRecipient rident)
                                             time
                                             (Optin ref)
                      ) = authHandler fbcbh sident rident time ref

handleMessaging fbcbh (CallbackMessagingDelivery (CallbackSender sident)
                                                 (CallbackRecipient rident)
                                                 delivery

                      ) = deliveryHandler fbcbh sident rident delivery

handleMessaging fbcbh (CallbackMessagingPostback (CallbackSender sident)
                                                 (CallbackRecipient rident)
                                                 time
                                                 (Postback payload)
                      ) = postbackHandler fbcbh sident rident time payload

handleMessaging fbcbh (CallbackMessagingAccountLink (CallbackSender sident)
                                                    (CallbackRecipient rident)
                                                    time
                                                    accountlink
                      ) = accountLinkHandler fbcbh sident rident time accountlink

handleMessaging fbcbh (CallbackMessagingRead (CallbackSender sident)
                                             (CallbackRecipient rident)
                                             time
                                             read'
                      ) = readHandler fbcbh sident rident time read'

handleMessaging fbcbh (CallbackMessagingEcho (CallbackSender sident)
                                             (CallbackRecipient rident)
                                             time
                                             echo
                      ) = echoHandler fbcbh sident rident time echo


senderAction :: SenderActionType
             -> RecipientID -> SenderActionRequest
senderAction typ = mkSenderAction typ . RecipientID

messageText :: [QuickReply] -> Maybe NotificationType
            -> Message
            -> RecipientID -> SendRequest
messageText quickreplies mtyp msg = mkMessageText quickreplies mtyp msg . RecipientID

attachmentImage :: [QuickReply] -> Maybe NotificationType
                -> Url -> Maybe Bool
                -> RecipientID -> SendRequest
attachmentImage quickreplies mtyp url reusable = mkAttachment quickreplies IMAGE mtyp url reusable . RecipientID

attachmentAudio :: [QuickReply] -> Maybe NotificationType
                -> Url -> Maybe Bool
                -> RecipientID -> SendRequest
attachmentAudio quickreplies mtyp url reusable = mkAttachment quickreplies AUDIO mtyp url reusable . RecipientID

attachmentVideo :: [QuickReply] -> Maybe NotificationType
                -> Url -> Maybe Bool
                -> RecipientID -> SendRequest
attachmentVideo quickreplies mtyp url reusable = mkAttachment quickreplies VIDEO mtyp url reusable . RecipientID

attachmentFile :: [QuickReply] -> Maybe NotificationType
               -> Url -> Maybe Bool
               -> RecipientID -> SendRequest
attachmentFile quickreplies mtyp url reusable = mkAttachment quickreplies FILE mtyp url reusable . RecipientID

genericTemplate :: [QuickReply] -> Maybe NotificationType
                -> [GenericTemplateElement]
                -> RecipientID -> SendRequest
genericTemplate quickreplies mtyp elems = mkGenericTemplate quickreplies mtyp elems . RecipientID

buttonTemplate :: Message -> Maybe NotificationType -> [TemplateButton] -> RecipientID -> SendRequest
buttonTemplate message mtyp elems = mkButtonTemplate message mtyp elems . RecipientID

listTemplate :: [QuickReply] -> Maybe NotificationType
             -> ListStyle
             -> [ListTemplateElement]
             -> Maybe TemplateButton
             -> RecipientID -> SendRequest
listTemplate quickreplies mtyp style elems mbutton =
    mkListTemplate quickreplies mtyp style elems mbutton . RecipientID

-- HelperFunctions to the HelperFunctions

mkSenderAction :: SenderActionType -> RequestRecipient -> SenderActionRequest
mkSenderAction action recipient = SenderActionRequest recipient action

mkMessageText :: [QuickReply] -> Maybe NotificationType
              -> Message
              -> RequestRecipient -> SendRequest
mkMessageText quickreplies notification message recipient =
    SendMessageRequest recipient
                       (RequestMessageText message $ mkQuickReplies quickreplies)
                       notification

mkAttachment :: [QuickReply] -> AttachmentType -> Maybe NotificationType
             -> Url
             -> Maybe Bool
             -> RequestRecipient -> SendRequest
mkAttachment quickreplies attachtype notification url reusable recipient =
    SendMessageRequest recipient
                       (RequestMessageAttachment 
                          (RequestMultimediaAttachment attachtype $ RequestMultimediaPayload url reusable)
                          $ mkQuickReplies quickreplies)
                       notification

mkGenericTemplate :: [QuickReply] -> Maybe NotificationType
                  -> [GenericTemplateElement]
                  -> RequestRecipient -> SendRequest
mkGenericTemplate quickreplies notification elements recipient =
    SendMessageRequest recipient
                       (RequestMessageAttachment
                          (RequestAttachmentTemplate
                              (GenericTemplatePayload elements))
                          $ mkQuickReplies quickreplies)
                       notification

mkButtonTemplate :: Message -> Maybe NotificationType
                 -> [TemplateButton]
                 -> RequestRecipient -> SendRequest
mkButtonTemplate message notification buttons recipient =
    SendMessageRequest recipient
                       (RequestMessageAttachment
                          (RequestAttachmentTemplate
                              (ButtonTemplatePayload message buttons))
                          Nothing)
                       notification

mkListTemplate :: [QuickReply] -> Maybe NotificationType
               -> ListStyle
               -> [ListTemplateElement]
               -> Maybe TemplateButton
               -> RequestRecipient -> SendRequest
mkListTemplate quickreplies notification style elements mbutton recipient =
    SendMessageRequest recipient
                       (RequestMessageAttachment
                          (RequestAttachmentTemplate
                            (ListTemplatePayload style elements mbutton))
                          $ mkQuickReplies quickreplies)
                        notification

{- SHOULD BE A BETTER WAY OF MAKING THIS EASIER, THOUGH NOT NEEDED YET
mkReceiptTemplate :: [QuickReply] -> RequestRecipient -> Maybe NotificationType -> FBRequestTemplatePayload -> SendRequest
mkReceiptTemplate quickreplies recipient notification receipt =
    SendMessageRequest recipient
                         (MessageAttachment
                            (AttachmentTemplate receipt)
                            $ mkQuickReplies quickreplies)
                         notification
-}

mkQuickReplies :: [QuickReply] -> Maybe [RequestQuickReply]
mkQuickReplies [] = Nothing
mkQuickReplies replies = Just $ fmap go replies
  where
    go (QR title payload image) = RequestQuickReply (Data.Text.take 20 title) payload image
    go (LocQR image)            = LocationQuickReply image


-- PHONE VARIANTS --

senderAction' :: SenderActionType -> RecipientPhone -> SenderActionRequest
senderAction' typ = mkSenderAction typ . RecipientPhone

messageText' :: [QuickReply] -> Maybe NotificationType -> Message -> RecipientPhone -> SendRequest
messageText' quickreplies mtyp msg = mkMessageText quickreplies mtyp msg . RecipientPhone

attachmentImage' :: [QuickReply] -> Maybe NotificationType -> Url -> Maybe Bool -> RecipientPhone -> SendRequest
attachmentImage' quickreplies mtyp url reusable = mkAttachment quickreplies IMAGE mtyp url reusable . RecipientPhone

attachmentAudio' :: [QuickReply] -> Maybe NotificationType -> Url -> Maybe Bool -> RecipientPhone -> SendRequest
attachmentAudio' quickreplies mtyp url reusable = mkAttachment quickreplies AUDIO mtyp url reusable . RecipientPhone

attachmentVideo' :: [QuickReply] -> Maybe NotificationType -> Url -> Maybe Bool -> RecipientPhone -> SendRequest
attachmentVideo' quickreplies mtyp url reusable = mkAttachment quickreplies VIDEO mtyp url reusable . RecipientPhone

attachmentFile' :: [QuickReply] -> Maybe NotificationType -> Url -> Maybe Bool -> RecipientPhone -> SendRequest
attachmentFile' quickreplies mtyp url reusable = mkAttachment quickreplies FILE mtyp url reusable . RecipientPhone

genericTemplate' :: [QuickReply] -> Maybe NotificationType -> [GenericTemplateElement] -> RecipientPhone -> SendRequest
genericTemplate' quickreplies mtyp elems = mkGenericTemplate quickreplies mtyp elems . RecipientPhone

buttonTemplate' :: Message -> Maybe NotificationType -> [TemplateButton] -> RecipientPhone -> SendRequest
buttonTemplate' msg mtyp buttons = mkButtonTemplate msg mtyp buttons . RecipientPhone

listTemplate' :: [QuickReply]
              -> Maybe NotificationType
              -> ListStyle
              -> [ListTemplateElement]
              -> Maybe TemplateButton
              -> RecipientID
              -> SendRequest
listTemplate' quickreplies mtyp style elems mbutton =
    mkListTemplate quickreplies mtyp style elems mbutton . RecipientPhone
