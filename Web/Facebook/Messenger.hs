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
    , messageTextQ
    , attachmentImageQ
    , attachmentAudioQ
    , attachmentVideoQ
    , attachmentFileQ
    , genericTemplateQ
    , buttonTemplateQ
    , module Web.Facebook.Messenger.Types
    -- All functions with prime ' use the RecipientPhone number instead of its PSID (Page-Scoped ID)
    , senderAction'
    , messageText'
    , attachmentImage'
    , attachmentAudio'
    , attachmentVideo'
    , attachmentFile'
    , genericTemplate'
    , messageTextQ'
    , attachmentImageQ'
    , attachmentAudioQ'
    , attachmentVideoQ'
    , attachmentFileQ'
    , genericTemplateQ'
    , buttonTemplate'
    , buttonTemplateQ'
    ) where

import Data.Text

import Web.Facebook.Messenger.Types



handleMessaging :: CallbackHandlers a -> CallbackMessaging -> a
handleMessaging fbcbh (CallbackMessagingMessage (CallbackSender sident)
                                                (CallbackRecipient rident)
                                                time
                                                (CallbackMessageText mid seq' message quickreply)
                      ) = messageHandler fbcbh sident rident time mid seq' message quickreply
                
handleMessaging fbcbh (CallbackMessagingMessage (CallbackSender sident)
                                                (CallbackRecipient rident)
                                                time
                                                (CallbackMessageAttachment mid seq' attachments)
                      ) = attachmentHandler fbcbh sident rident time mid seq' attachments

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


senderAction :: SenderActionType -> RecipientID -> SenderActionRequest
senderAction typ = mkSenderAction typ . RecipientID

messageText :: Maybe NotificationType -> Message -> RecipientID -> SendRequest
messageText mtyp msg = mkMessageText [] mtyp msg . RecipientID

messageTextQ :: [(Text,Text)] -> Maybe NotificationType -> Message -> RecipientID -> SendRequest
messageTextQ quickreplies mtyp msg = mkMessageText quickreplies mtyp msg . RecipientID

attachmentImage :: Maybe NotificationType -> Url -> RecipientID -> SendRequest
attachmentImage mtyp url = mkAttachment [] IMAGE mtyp url . RecipientID

attachmentImageQ :: [(Text,Text)] -> Maybe NotificationType -> Url -> RecipientID -> SendRequest
attachmentImageQ quickreplies mtyp url = mkAttachment quickreplies IMAGE mtyp url . RecipientID

attachmentAudio :: Maybe NotificationType -> Url -> RecipientID -> SendRequest
attachmentAudio mtyp url = mkAttachment [] AUDIO mtyp url . RecipientID

attachmentAudioQ :: [(Text,Text)] -> Maybe NotificationType -> Url -> RecipientID -> SendRequest
attachmentAudioQ quickreplies mtyp url = mkAttachment quickreplies AUDIO mtyp url . RecipientID

attachmentVideo :: Maybe NotificationType -> Url -> RecipientID -> SendRequest
attachmentVideo mtyp url = mkAttachment [] VIDEO mtyp url . RecipientID

attachmentVideoQ :: [(Text,Text)] -> Maybe NotificationType -> Url -> RecipientID -> SendRequest
attachmentVideoQ quickreplies mtyp url = mkAttachment quickreplies VIDEO mtyp url . RecipientID

attachmentFile :: Maybe NotificationType -> Url -> RecipientID -> SendRequest
attachmentFile mtyp url = mkAttachment [] FILE mtyp url . RecipientID

attachmentFileQ :: [(Text,Text)] -> Maybe NotificationType -> Url -> RecipientID -> SendRequest
attachmentFileQ quickreplies mtyp url = mkAttachment quickreplies FILE mtyp url . RecipientID

genericTemplate :: Maybe NotificationType -> [GenericTemplateElement] -> RecipientID -> SendRequest
genericTemplate mtyp elems = mkGenericTemplate [] mtyp elems . RecipientID

genericTemplateQ :: [(Text,Text)] -> Maybe NotificationType -> [GenericTemplateElement] -> RecipientID -> SendRequest
genericTemplateQ quickreplies mtyp elems = mkGenericTemplate quickreplies mtyp elems . RecipientID

buttonTemplate :: Message -> Maybe NotificationType -> [TemplateButton] -> RecipientID -> SendRequest
buttonTemplate message mtyp elems = mkButtonTemplate [] message mtyp elems . RecipientID

buttonTemplateQ :: [(Text,Text)] -> Message -> Maybe NotificationType -> [TemplateButton] -> RecipientID -> SendRequest
buttonTemplateQ quickreplies message mtyp elems = mkButtonTemplate quickreplies message mtyp elems . RecipientID

-- HelperFunctions to the HelperFunctions

mkSenderAction :: SenderActionType -> RequestRecipient -> SenderActionRequest
mkSenderAction action recipient = SenderActionRequest recipient action

mkMessageText :: [(Text,Text)] -> Maybe NotificationType -> Message -> RequestRecipient -> SendRequest
mkMessageText quickreplies notification message recipient =
    SendMessageRequest recipient
                       (RequestMessageText message $ mkQuickReplies quickreplies)
                       notification

mkAttachment :: [(Text,Text)] -> AttachmentType -> Maybe NotificationType -> Url -> RequestRecipient -> SendRequest
mkAttachment quickreplies attachtype notification url recipient =
    SendMessageRequest recipient
                       (RequestMessageAttachment 
                          (RequestMultimediaAttachment attachtype $ RequestMultimediaPayload url)
                          $ mkQuickReplies quickreplies)
                       notification

mkGenericTemplate :: [(Text,Text)] -> Maybe NotificationType -> [GenericTemplateElement] -> RequestRecipient -> SendRequest
mkGenericTemplate quickreplies notification elements recipient =
    SendMessageRequest recipient
                       (RequestMessageAttachment
                          (RequestAttachmentTemplate
                              (GenericTemplatePayload elements))
                          $ mkQuickReplies quickreplies)
                       notification

mkButtonTemplate :: [(Text,Text)] -> Message -> Maybe NotificationType -> [TemplateButton] -> RequestRecipient -> SendRequest
mkButtonTemplate quickreplies message notification buttons recipient =
    SendMessageRequest recipient
                       (RequestMessageAttachment
                          (RequestAttachmentTemplate
                              (ButtonTemplatePayload message buttons))
                          $ mkQuickReplies quickreplies)
                       notification

{- SHOULD BE A BETTER WAY OF MAKING THIS EASIER, THOUGH NOT NEEDED YET
mkReceiptTemplate :: [(Text,Text)] -> RequestRecipient -> Maybe NotificationType -> FBRequestTemplatePayload -> SendRequest
mkReceiptTemplate quickreplies recipient notification receipt =
    SendMessageRequest recipient
                         (MessageAttachment
                            (AttachmentTemplate receipt)
                            $ mkQuickReplies quickreplies)
                         notification
-}

mkQuickReplies :: [(Text,Text)] -> Maybe [RequestQuickReply]
mkQuickReplies [] = Nothing
mkQuickReplies replies = Just $ fmap go replies
  where
    go (title,payload) = RequestQuickReply title payload


-- PHONE VARIANTS --

senderAction' :: SenderActionType -> RecipientPhone -> SenderActionRequest
senderAction' typ = mkSenderAction typ . RecipientPhone

messageText' :: Maybe NotificationType -> Message -> RecipientPhone -> SendRequest
messageText' mtyp msg = mkMessageText [] mtyp msg . RecipientPhone

attachmentImage' :: Maybe NotificationType -> Url -> RecipientPhone -> SendRequest
attachmentImage' mtyp url = mkAttachment [] IMAGE mtyp url . RecipientPhone

attachmentAudio' :: Maybe NotificationType -> Url -> RecipientPhone -> SendRequest
attachmentAudio' mtyp url = mkAttachment [] AUDIO mtyp url . RecipientPhone

attachmentVideo' :: Maybe NotificationType -> Url -> RecipientPhone -> SendRequest
attachmentVideo' mtyp url = mkAttachment [] VIDEO mtyp url . RecipientPhone

attachmentFile' :: Maybe NotificationType -> Url -> RecipientPhone -> SendRequest
attachmentFile' mtyp url = mkAttachment [] FILE mtyp url . RecipientPhone

genericTemplate' :: Maybe NotificationType -> [GenericTemplateElement] -> RecipientPhone -> SendRequest
genericTemplate' mtyp elems = mkGenericTemplate [] mtyp elems . RecipientPhone

buttonTemplate' :: Message -> Maybe NotificationType -> [TemplateButton] -> RecipientPhone -> SendRequest
buttonTemplate' msg mtyp buttons = mkButtonTemplate [] msg mtyp buttons . RecipientPhone

messageTextQ' :: [(Text,Text)] -> Maybe NotificationType -> Message -> RecipientPhone -> SendRequest
messageTextQ' quickreplies mtyp msg = mkMessageText quickreplies mtyp msg . RecipientPhone

attachmentImageQ' :: [(Text,Text)] -> Maybe NotificationType -> Url -> RecipientPhone -> SendRequest
attachmentImageQ' quickreplies mtyp url = mkAttachment quickreplies IMAGE mtyp url . RecipientPhone

attachmentAudioQ' :: [(Text,Text)] -> Maybe NotificationType -> Url -> RecipientPhone -> SendRequest
attachmentAudioQ' quickreplies mtyp url = mkAttachment quickreplies AUDIO mtyp url . RecipientPhone

attachmentVideoQ' :: [(Text,Text)] -> Maybe NotificationType -> Url -> RecipientPhone -> SendRequest
attachmentVideoQ' quickreplies mtyp url = mkAttachment quickreplies VIDEO mtyp url . RecipientPhone

attachmentFileQ' :: [(Text,Text)] -> Maybe NotificationType -> Url -> RecipientPhone -> SendRequest
attachmentFileQ' quickreplies mtyp url = mkAttachment quickreplies FILE mtyp url . RecipientPhone

genericTemplateQ' :: [(Text,Text)] -> Maybe NotificationType -> [GenericTemplateElement] -> RecipientPhone -> SendRequest
genericTemplateQ' quickreplies mtyp elems = mkGenericTemplate quickreplies mtyp elems . RecipientPhone

buttonTemplateQ' :: [(Text,Text)] -> Message -> Maybe NotificationType -> [TemplateButton] -> RecipientPhone -> SendRequest
buttonTemplateQ' quickreplies msg mtyp buttons = mkButtonTemplate quickreplies msg mtyp buttons . RecipientPhone
