module Web.Facebook.Messenger
    ( handleFacebookMessaging
    , mkFBSenderAction
    , mkFBMessageText
    , mkFBAttachmentImage
    , mkFBAttachmentAudio
    , mkFBAttachmentVideo
    , mkFBAttachmentFile
    , mkFBGenericTemplate
    , mkFBButtonTemplate
    , mkFBMessageTextQ
    , mkFBAttachmentImageQ
    , mkFBAttachmentAudioQ
    , mkFBAttachmentVideoQ
    , mkFBAttachmentFileQ
    , mkFBGenericTemplateQ
    , mkFBButtonTemplateQ
    , module Web.Facebook.Messenger.Types
    -- All functions with prime ' use the RecipientPhone number instead of its PSID (Page-Scoped ID)
    , mkFBSenderAction'
    , mkFBMessageText'
    , mkFBAttachmentImage'
    , mkFBAttachmentAudio'
    , mkFBAttachmentVideo'
    , mkFBAttachmentFile'
    , mkFBGenericTemplate'
    , mkFBMessageTextQ'
    , mkFBAttachmentImageQ'
    , mkFBAttachmentAudioQ'
    , mkFBAttachmentVideoQ'
    , mkFBAttachmentFileQ'
    , mkFBGenericTemplateQ'
    , mkFBButtonTemplate'
    , mkFBButtonTemplateQ'
    ) where

import Data.Text

import Web.Facebook.Messenger.Types



handleFacebookMessaging :: FacebookCallbackHandlers m a -> FBCallbackMessaging -> m a
handleFacebookMessaging fbcbh (FBCallbackMessagingMessage (FBCallbackSender sident)
                                                          (FBCallbackRecipient rident)
                                                          time
                                                          (FBCallbackMessage mid seq' message quickreply)
                              ) = fb_messageHandler fbcbh sident rident time mid seq' message quickreply
                
handleFacebookMessaging fbcbh (FBCallbackMessagingMessage (FBCallbackSender sident)
                                                          (FBCallbackRecipient rident)
                                                          time
                                                          (FBCallbackAttachment mid seq' attachments)
                              ) = fb_attachmentHandler fbcbh sident rident time mid seq' attachments

handleFacebookMessaging fbcbh (FBCallbackMessagingAuth (FBCallbackSender sident)
                                                       (FBCallbackRecipient rident)
                                                       time
                                                       (FBCallbackOptin ref)
                              ) = fb_authHandler fbcbh sident rident time ref

handleFacebookMessaging fbcbh (FBCallbackMessagingDelivery (FBCallbackSender sident)
                                                           (FBCallbackRecipient rident)
                                                           delivery
                              ) = fb_deliveryHandler fbcbh sident rident delivery

handleFacebookMessaging fbcbh (FBCallbackMessagingPostback (FBCallbackSender sident)
                                                           (FBCallbackRecipient rident)
                                                           time
                                                           (FBCallbackPostback payload)
                              ) = fb_postbackHandler fbcbh sident rident time payload

handleFacebookMessaging fbcbh (FBCallbackMessagingAccountLink (FBCallbackSender sident)
                                                              (FBCallbackRecipient rident)
                                                              time
                                                              accountlink
                              ) = fb_accountLinkHandler fbcbh sident rident time accountlink

handleFacebookMessaging fbcbh (FBCallbackMessagingRead (FBCallbackSender sident)
                                                       (FBCallbackRecipient rident)
                                                       time
                                                       read'
                              ) = fb_readHandler fbcbh sident rident time read'

handleFacebookMessaging fbcbh (FBCallbackMessagingEcho (FBCallbackSender sident)
                                                       (FBCallbackRecipient rident)
                                                       time
                                                       echo
                              ) = fb_echoHandler fbcbh sident rident time echo


mkFBSenderAction :: RecipientID -> FBRequestSenderActionType -> FBSendRequest
mkFBSenderAction = mkSenderAction . FBRequestRecipientID

mkFBMessageText :: RecipientID -> Maybe FBRequestNotificationType -> Message -> FBSendRequest
mkFBMessageText = mkMessageText [] . FBRequestRecipientID

mkFBMessageTextQ :: [(Text,Text)] -> RecipientID -> Maybe FBRequestNotificationType -> Message -> FBSendRequest
mkFBMessageTextQ quickreplies = mkMessageText quickreplies . FBRequestRecipientID

mkFBAttachmentImage :: RecipientID -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentImage = mkAttachment [] IMAGE . FBRequestRecipientID

mkFBAttachmentImageQ :: [(Text,Text)] -> RecipientID -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentImageQ quickreplies = mkAttachment quickreplies IMAGE . FBRequestRecipientID

mkFBAttachmentAudio :: RecipientID -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentAudio = mkAttachment [] AUDIO . FBRequestRecipientID

mkFBAttachmentAudioQ :: [(Text,Text)] -> RecipientID -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentAudioQ quickreplies = mkAttachment quickreplies AUDIO . FBRequestRecipientID

mkFBAttachmentVideo :: RecipientID -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentVideo = mkAttachment [] VIDEO . FBRequestRecipientID

mkFBAttachmentVideoQ :: [(Text,Text)] -> RecipientID -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentVideoQ quickreplies = mkAttachment quickreplies VIDEO . FBRequestRecipientID

mkFBAttachmentFile :: RecipientID -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentFile = mkAttachment [] FILE . FBRequestRecipientID

mkFBAttachmentFileQ :: [(Text,Text)] -> RecipientID -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentFileQ quickreplies = mkAttachment quickreplies FILE . FBRequestRecipientID

mkFBGenericTemplate :: RecipientID -> Maybe FBRequestNotificationType -> [FBRequestGenericTemplateElement] -> FBSendRequest
mkFBGenericTemplate = mkGenericTemplate [] . FBRequestRecipientID

mkFBGenericTemplateQ :: [(Text,Text)] -> RecipientID -> Maybe FBRequestNotificationType -> [FBRequestGenericTemplateElement] -> FBSendRequest
mkFBGenericTemplateQ quickreplies = mkGenericTemplate quickreplies . FBRequestRecipientID

mkFBButtonTemplate :: RecipientID -> Message -> Maybe FBRequestNotificationType -> [FBRequestTemplateButton] -> FBSendRequest
mkFBButtonTemplate = mkButtonTemplate [] . FBRequestRecipientID

mkFBButtonTemplateQ :: [(Text,Text)] -> RecipientID -> Message -> Maybe FBRequestNotificationType -> [FBRequestTemplateButton] -> FBSendRequest
mkFBButtonTemplateQ quickreplies = mkButtonTemplate quickreplies . FBRequestRecipientID

-- HelperFunctions to the HelperFunctions

mkSenderAction :: FBRequestRecipient -> FBRequestSenderActionType -> FBSendRequest
mkSenderAction recipient action = FBSenderActionRequest recipient action

mkMessageText :: [(Text,Text)] -> FBRequestRecipient -> Maybe FBRequestNotificationType -> Message -> FBSendRequest
mkMessageText quickreplies recipient notification message =
    FBSendMessageRequest recipient
                         (FBRequestMessageText message $ mkQuickReplies quickreplies)
                         notification

mkAttachment :: [(Text,Text)] -> FBRequestAttachmentType -> FBRequestRecipient -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkAttachment quickreplies attachtype recipient notification url =
    FBSendMessageRequest recipient
                         (FBRequestMessageAttachment 
                            (FBRequestMultimediaAttachment attachtype $ FBRequestMultimediaPayload url)
                            $ mkQuickReplies quickreplies)
                         notification

mkGenericTemplate :: [(Text,Text)] -> FBRequestRecipient -> Maybe FBRequestNotificationType -> [FBRequestGenericTemplateElement] -> FBSendRequest
mkGenericTemplate quickreplies recipient notification elements =
    FBSendMessageRequest recipient
                         (FBRequestMessageAttachment
                            (FBRequestAttachmentTemplate
                                (FBRequestGenericTemplatePayload elements))
                            $ mkQuickReplies quickreplies)
                         notification

mkButtonTemplate :: [(Text,Text)] -> FBRequestRecipient -> Message -> Maybe FBRequestNotificationType -> [FBRequestTemplateButton] -> FBSendRequest
mkButtonTemplate quickreplies recipient message notification buttons =
    FBSendMessageRequest recipient
                         (FBRequestMessageAttachment
                            (FBRequestAttachmentTemplate
                                (FBRequestButtonTemplatePayload message buttons))
                            $ mkQuickReplies quickreplies)
                         notification

{- SHOULD BE A BETTER WAY OF MAKING THIS EASIER, THOUGH NOT NEEDED YET
mkReceiptTemplate :: [(Text,Text)] -> FBRequestRecipient -> Maybe FBRequestNotificationType -> FBRequestTemplatePayload -> FBSendRequest
mkReceiptTemplate quickreplies recipient notification receipt =
    FBSendMessageRequest recipient
                         (FBRequestMessageAttachment
                            (FBRequestAttachmentTemplate receipt)
                            $ mkQuickReplies quickreplies)
                         notification
-}

mkQuickReplies :: [(Text,Text)] -> Maybe [FBRequestQuickReply]
mkQuickReplies [] = Nothing
mkQuickReplies replies = Just $ fmap go replies
  where
    go (title,payload) = FBRequestQuickReply title payload


-- PHONE VARIANTS --

mkFBSenderAction' :: RecipientPhone -> FBRequestSenderActionType -> FBSendRequest
mkFBSenderAction' = mkSenderAction . FBRequestRecipientPhone

mkFBMessageText' :: RecipientPhone -> Maybe FBRequestNotificationType -> Message -> FBSendRequest
mkFBMessageText' = mkMessageText [] . FBRequestRecipientPhone

mkFBAttachmentImage' :: RecipientPhone -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentImage' = mkAttachment [] IMAGE . FBRequestRecipientPhone

mkFBAttachmentAudio' :: RecipientPhone -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentAudio' = mkAttachment [] AUDIO . FBRequestRecipientPhone

mkFBAttachmentVideo' :: RecipientPhone -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentVideo' = mkAttachment [] VIDEO . FBRequestRecipientPhone

mkFBAttachmentFile' :: RecipientPhone -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentFile' = mkAttachment [] FILE . FBRequestRecipientPhone

mkFBGenericTemplate' :: RecipientPhone -> Maybe FBRequestNotificationType -> [FBRequestGenericTemplateElement] -> FBSendRequest
mkFBGenericTemplate' = mkGenericTemplate [] . FBRequestRecipientPhone

mkFBMessageTextQ' :: [(Text,Text)] -> RecipientPhone -> Maybe FBRequestNotificationType -> Message -> FBSendRequest
mkFBMessageTextQ' quickreplies = mkMessageText quickreplies . FBRequestRecipientPhone

mkFBAttachmentImageQ' :: [(Text,Text)] -> RecipientPhone -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentImageQ' quickreplies = mkAttachment quickreplies IMAGE . FBRequestRecipientPhone

mkFBAttachmentAudioQ' :: [(Text,Text)] -> RecipientPhone -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentAudioQ' quickreplies = mkAttachment quickreplies AUDIO . FBRequestRecipientPhone

mkFBAttachmentVideoQ' :: [(Text,Text)] -> RecipientPhone -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentVideoQ' quickreplies = mkAttachment quickreplies VIDEO . FBRequestRecipientPhone

mkFBAttachmentFileQ' :: [(Text,Text)] -> RecipientPhone -> Maybe FBRequestNotificationType -> Url -> FBSendRequest
mkFBAttachmentFileQ' quickreplies = mkAttachment quickreplies FILE . FBRequestRecipientPhone

mkFBGenericTemplateQ' :: [(Text,Text)] -> RecipientPhone -> Maybe FBRequestNotificationType -> [FBRequestGenericTemplateElement] -> FBSendRequest
mkFBGenericTemplateQ' quickreplies = mkGenericTemplate quickreplies . FBRequestRecipientPhone

mkFBButtonTemplate' :: RecipientPhone -> Message -> Maybe FBRequestNotificationType -> [FBRequestTemplateButton] -> FBSendRequest
mkFBButtonTemplate' = mkButtonTemplate [] . FBRequestRecipientPhone

mkFBButtonTemplateQ' :: [(Text,Text)] -> RecipientPhone -> Message -> Maybe FBRequestNotificationType -> [FBRequestTemplateButton] -> FBSendRequest
mkFBButtonTemplateQ' quickreplies = mkButtonTemplate quickreplies . FBRequestRecipientPhone
