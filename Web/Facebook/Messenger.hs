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
    -- All functions with -Ref use the RecipientRef value instead of its PSID
    -- The moment -Ref had been used the response of Facebook should contain the PSID to be used after that
    , senderActionRef
    , messageTextRef
    , attachmentImageRef
    , attachmentAudioRef
    , attachmentVideoRef
    , attachmentFileRef
    , genericTemplateRef
    , buttonTemplateRef
    , listTemplateRef
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
handleMessaging fbcbh (CallbackMessagingOptin (CallbackSender sident)
                                              (CallbackRecipient rident)
                                              time
                                              (Optin ref)
                      ) = optinHandler fbcbh sident rident time ref

handleMessaging fbcbh (CallbackMessagingOptinRef (CallbackRecipient rident)
                                                 time
                                                 (OptinRef ref user_ref)
                      ) = optinRefHandler fbcbh rident time ref user_ref

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

attachmentImage
  , attachmentAudio
  , attachmentVideo
  , attachmentFile :: [QuickReply] -> Maybe NotificationType
                   -> Url -> Bool
                   -> RecipientID -> SendRequest
attachmentImage quickreplies mtyp url reusable = mkAttachment IMAGE quickreplies mtyp url reusable . RecipientID
attachmentAudio quickreplies mtyp url reusable = mkAttachment AUDIO quickreplies mtyp url reusable . RecipientID
attachmentVideo quickreplies mtyp url reusable = mkAttachment VIDEO quickreplies mtyp url reusable . RecipientID
attachmentFile  quickreplies mtyp url reusable = mkAttachment FILE  quickreplies mtyp url reusable . RecipientID

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
    SendRequest recipient
                (RequestMessageText message $ mkQuickReplies quickreplies)
                notification

mkAttachment :: AttachmentType -> [QuickReply] -> Maybe NotificationType
             -> Url -> Bool
             -> RequestRecipient -> SendRequest
mkAttachment attachtype quickreplies notification url reusable recipient =
    SendRequest recipient
                (RequestMessageAttachment 
                   (RequestMultimediaAttachment attachtype $ RequestMultimediaPayload url reusable)
                   $ mkQuickReplies quickreplies)
                notification

mkGenericTemplate :: [QuickReply] -> Maybe NotificationType
                  -> [GenericTemplateElement]
                  -> RequestRecipient -> SendRequest
mkGenericTemplate quickreplies notification elements recipient =
    SendRequest recipient
                (RequestMessageAttachment
                   (RequestAttachmentTemplate
                       (GenericTemplatePayload elements))
                   $ mkQuickReplies quickreplies)
                notification

mkButtonTemplate :: Message -> Maybe NotificationType
                 -> [TemplateButton]
                 -> RequestRecipient -> SendRequest
mkButtonTemplate message notification buttons recipient =
    SendRequest recipient
                (RequestMessageAttachment
                   (RequestAttachmentTemplate
                       (ButtonTemplatePayload message buttons))
                   [])
                notification

mkListTemplate :: [QuickReply] -> Maybe NotificationType
               -> ListStyle
               -> [ListTemplateElement]
               -> Maybe TemplateButton
               -> RequestRecipient -> SendRequest
mkListTemplate quickreplies notification style elements mbutton recipient =
    SendRequest recipient
                (RequestMessageAttachment
                   (RequestAttachmentTemplate
                     (ListTemplatePayload style elements mbutton))
                   $ mkQuickReplies quickreplies)
                 notification

{- SHOULD BE A BETTER WAY OF MAKING THIS EASIER, THOUGH NOT NEEDED YET
mkReceiptTemplate :: [QuickReply] -> RequestRecipient -> Maybe NotificationType -> FBRequestTemplatePayload -> SendRequest
mkReceiptTemplate quickreplies recipient notification receipt =
    SendRequest recipient
                         (MessageAttachment
                            (AttachmentTemplate receipt)
                            $ mkQuickReplies quickreplies)
                         notification
-}

mkQuickReplies :: [QuickReply] -> [RequestQuickReply]
mkQuickReplies replies = fmap go replies
  where
    go (QR title payload image) = RequestQuickReply (Data.Text.take 20 title) payload image
    go (LocQR image)            = LocationQuickReply image


-- -------------------------- --
--  RecipientPhone Functions  -- 
-- -------------------------- --

senderAction' :: SenderActionType -> RecipientPhone -> SenderActionRequest
senderAction' typ = mkSenderAction typ . RecipientPhone

messageText' :: [QuickReply] -> Maybe NotificationType -> Message -> RecipientPhone -> SendRequest
messageText' quickreplies mtyp msg = mkMessageText quickreplies mtyp msg . RecipientPhone

attachmentImage'
  , attachmentAudio'
  , attachmentVideo'
  , attachmentFile' :: [QuickReply] -> Maybe NotificationType -> Url -> Bool -> RecipientPhone -> SendRequest

attachmentImage' quickreplies mtyp url reusable = mkAttachment IMAGE quickreplies mtyp url reusable . RecipientPhone
attachmentAudio' quickreplies mtyp url reusable = mkAttachment AUDIO quickreplies mtyp url reusable . RecipientPhone
attachmentVideo' quickreplies mtyp url reusable = mkAttachment VIDEO quickreplies mtyp url reusable . RecipientPhone
attachmentFile'  quickreplies mtyp url reusable = mkAttachment FILE  quickreplies mtyp url reusable . RecipientPhone

genericTemplate' :: [QuickReply] -> Maybe NotificationType -> [GenericTemplateElement] -> RecipientPhone -> SendRequest
genericTemplate' quickreplies mtyp elems = mkGenericTemplate quickreplies mtyp elems . RecipientPhone

buttonTemplate' :: Message -> Maybe NotificationType -> [TemplateButton] -> RecipientPhone -> SendRequest
buttonTemplate' msg mtyp buttons = mkButtonTemplate msg mtyp buttons . RecipientPhone

listTemplate' :: [QuickReply]
              -> Maybe NotificationType
              -> ListStyle
              -> [ListTemplateElement]
              -> Maybe TemplateButton
              -> RecipientPhone
              -> SendRequest
listTemplate' quickreplies mtyp style elems mbutton =
    mkListTemplate quickreplies mtyp style elems mbutton . RecipientPhone

-- ------------------------ --
--  RecipientRef Functions  -- 
-- ------------------------ --

senderActionRef :: SenderActionType -> RecipientRef -> SenderActionRequest
senderActionRef typ = mkSenderAction typ . RecipientRef

messageTextRef :: [QuickReply] -> Maybe NotificationType -> Message -> RecipientRef -> SendRequest
messageTextRef quickreplies mtyp msg = mkMessageText quickreplies mtyp msg . RecipientRef

attachmentImageRef
  , attachmentAudioRef
  , attachmentVideoRef
  , attachmentFileRef :: [QuickReply] -> Maybe NotificationType -> Url -> Bool -> RecipientRef -> SendRequest

attachmentImageRef quickreplies mtyp url reusable = mkAttachment IMAGE quickreplies mtyp url reusable . RecipientRef
attachmentAudioRef quickreplies mtyp url reusable = mkAttachment AUDIO quickreplies mtyp url reusable . RecipientRef
attachmentVideoRef quickreplies mtyp url reusable = mkAttachment VIDEO quickreplies mtyp url reusable . RecipientRef
attachmentFileRef  quickreplies mtyp url reusable = mkAttachment FILE  quickreplies mtyp url reusable . RecipientRef

genericTemplateRef :: [QuickReply] -> Maybe NotificationType -> [GenericTemplateElement] -> RecipientRef -> SendRequest
genericTemplateRef quickreplies mtyp elems = mkGenericTemplate quickreplies mtyp elems . RecipientRef

buttonTemplateRef :: Message -> Maybe NotificationType -> [TemplateButton] -> RecipientRef -> SendRequest
buttonTemplateRef msg mtyp buttons = mkButtonTemplate msg mtyp buttons . RecipientRef

listTemplateRef :: [QuickReply]
              -> Maybe NotificationType
              -> ListStyle
              -> [ListTemplateElement]
              -> Maybe TemplateButton
              -> RecipientRef
              -> SendRequest
listTemplateRef quickreplies mtyp style elems mbutton =
    mkListTemplate quickreplies mtyp style elems mbutton . RecipientRef
