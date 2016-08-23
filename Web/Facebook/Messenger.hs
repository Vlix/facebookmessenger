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



handleFacebookMessaging :: FacebookCallbackHandlers a -> FBCallbackMessaging -> a
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


mkFBSenderAction :: FBRequestSenderActionType -> RecipientID -> FBSendRequest
mkFBSenderAction typ = mkSenderAction typ . FBRequestRecipientID

mkFBMessageText :: Maybe FBRequestNotificationType -> Message -> RecipientID -> FBSendRequest
mkFBMessageText mtyp msg = mkMessageText [] mtyp msg . FBRequestRecipientID

mkFBMessageTextQ :: [(Text,Text)] -> Maybe FBRequestNotificationType -> Message -> RecipientID -> FBSendRequest
mkFBMessageTextQ quickreplies mtyp msg = mkMessageText quickreplies mtyp msg . FBRequestRecipientID

mkFBAttachmentImage :: Maybe FBRequestNotificationType -> Url -> RecipientID -> FBSendRequest
mkFBAttachmentImage mtyp url = mkAttachment [] IMAGE mtyp url . FBRequestRecipientID

mkFBAttachmentImageQ :: [(Text,Text)] -> Maybe FBRequestNotificationType -> Url -> RecipientID -> FBSendRequest
mkFBAttachmentImageQ quickreplies mtyp url = mkAttachment quickreplies IMAGE mtyp url . FBRequestRecipientID

mkFBAttachmentAudio :: Maybe FBRequestNotificationType -> Url -> RecipientID -> FBSendRequest
mkFBAttachmentAudio mtyp url = mkAttachment [] AUDIO mtyp url . FBRequestRecipientID

mkFBAttachmentAudioQ :: [(Text,Text)] -> Maybe FBRequestNotificationType -> Url -> RecipientID -> FBSendRequest
mkFBAttachmentAudioQ quickreplies mtyp url = mkAttachment quickreplies AUDIO mtyp url . FBRequestRecipientID

mkFBAttachmentVideo :: Maybe FBRequestNotificationType -> Url -> RecipientID -> FBSendRequest
mkFBAttachmentVideo mtyp url = mkAttachment [] VIDEO mtyp url . FBRequestRecipientID

mkFBAttachmentVideoQ :: [(Text,Text)] -> Maybe FBRequestNotificationType -> Url -> RecipientID -> FBSendRequest
mkFBAttachmentVideoQ quickreplies mtyp url = mkAttachment quickreplies VIDEO mtyp url . FBRequestRecipientID

mkFBAttachmentFile :: Maybe FBRequestNotificationType -> Url -> RecipientID -> FBSendRequest
mkFBAttachmentFile mtyp url = mkAttachment [] FILE mtyp url . FBRequestRecipientID

mkFBAttachmentFileQ :: [(Text,Text)] -> Maybe FBRequestNotificationType -> Url -> RecipientID -> FBSendRequest
mkFBAttachmentFileQ quickreplies mtyp url = mkAttachment quickreplies FILE mtyp url . FBRequestRecipientID

mkFBGenericTemplate :: Maybe FBRequestNotificationType -> [FBRequestGenericTemplateElement] -> RecipientID -> FBSendRequest
mkFBGenericTemplate mtyp elems = mkGenericTemplate [] mtyp elems . FBRequestRecipientID

mkFBGenericTemplateQ :: [(Text,Text)] -> Maybe FBRequestNotificationType -> [FBRequestGenericTemplateElement] -> RecipientID -> FBSendRequest
mkFBGenericTemplateQ quickreplies mtyp elems = mkGenericTemplate quickreplies mtyp elems . FBRequestRecipientID

mkFBButtonTemplate :: Message -> Maybe FBRequestNotificationType -> [FBRequestTemplateButton] -> RecipientID -> FBSendRequest
mkFBButtonTemplate message mtyp elems = mkButtonTemplate [] message mtyp elems . FBRequestRecipientID

mkFBButtonTemplateQ :: [(Text,Text)] -> Message -> Maybe FBRequestNotificationType -> [FBRequestTemplateButton] -> RecipientID -> FBSendRequest
mkFBButtonTemplateQ quickreplies message mtyp elems = mkButtonTemplate quickreplies message mtyp elems . FBRequestRecipientID

-- HelperFunctions to the HelperFunctions

mkSenderAction :: FBRequestSenderActionType -> FBRequestRecipient -> FBSendRequest
mkSenderAction action recipient = FBSenderActionRequest recipient action

mkMessageText :: [(Text,Text)] -> Maybe FBRequestNotificationType -> Message -> FBRequestRecipient -> FBSendRequest
mkMessageText quickreplies notification message recipient =
    FBSendMessageRequest recipient
                         (FBRequestMessageText message $ mkQuickReplies quickreplies)
                         notification

mkAttachment :: [(Text,Text)] -> FBAttachmentType -> Maybe FBRequestNotificationType -> Url -> FBRequestRecipient -> FBSendRequest
mkAttachment quickreplies attachtype notification url recipient =
    FBSendMessageRequest recipient
                         (FBRequestMessageAttachment 
                            (FBRequestMultimediaAttachment attachtype $ FBRequestMultimediaPayload url)
                            $ mkQuickReplies quickreplies)
                         notification

mkGenericTemplate :: [(Text,Text)] -> Maybe FBRequestNotificationType -> [FBRequestGenericTemplateElement] -> FBRequestRecipient -> FBSendRequest
mkGenericTemplate quickreplies notification elements recipient =
    FBSendMessageRequest recipient
                         (FBRequestMessageAttachment
                            (FBRequestAttachmentTemplate
                                (FBRequestGenericTemplatePayload elements))
                            $ mkQuickReplies quickreplies)
                         notification

mkButtonTemplate :: [(Text,Text)] -> Message -> Maybe FBRequestNotificationType -> [FBRequestTemplateButton] -> FBRequestRecipient -> FBSendRequest
mkButtonTemplate quickreplies message notification buttons recipient =
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

mkFBSenderAction' :: FBRequestSenderActionType -> RecipientPhone -> FBSendRequest
mkFBSenderAction' typ = mkSenderAction typ . FBRequestRecipientPhone

mkFBMessageText' :: Maybe FBRequestNotificationType -> Message -> RecipientPhone -> FBSendRequest
mkFBMessageText' mtyp msg = mkMessageText [] mtyp msg . FBRequestRecipientPhone

mkFBAttachmentImage' :: Maybe FBRequestNotificationType -> Url -> RecipientPhone -> FBSendRequest
mkFBAttachmentImage' mtyp url = mkAttachment [] IMAGE mtyp url . FBRequestRecipientPhone

mkFBAttachmentAudio' :: Maybe FBRequestNotificationType -> Url -> RecipientPhone -> FBSendRequest
mkFBAttachmentAudio' mtyp url = mkAttachment [] AUDIO mtyp url . FBRequestRecipientPhone

mkFBAttachmentVideo' :: Maybe FBRequestNotificationType -> Url -> RecipientPhone -> FBSendRequest
mkFBAttachmentVideo' mtyp url = mkAttachment [] VIDEO mtyp url . FBRequestRecipientPhone

mkFBAttachmentFile' :: Maybe FBRequestNotificationType -> Url -> RecipientPhone -> FBSendRequest
mkFBAttachmentFile' mtyp url = mkAttachment [] FILE mtyp url . FBRequestRecipientPhone

mkFBGenericTemplate' :: Maybe FBRequestNotificationType -> [FBRequestGenericTemplateElement] -> RecipientPhone -> FBSendRequest
mkFBGenericTemplate' mtyp elems = mkGenericTemplate [] mtyp elems . FBRequestRecipientPhone

mkFBButtonTemplate' :: Message -> Maybe FBRequestNotificationType -> [FBRequestTemplateButton] -> RecipientPhone -> FBSendRequest
mkFBButtonTemplate' msg mtyp buttons = mkButtonTemplate [] msg mtyp buttons . FBRequestRecipientPhone

mkFBMessageTextQ' :: [(Text,Text)] -> Maybe FBRequestNotificationType -> Message -> RecipientPhone -> FBSendRequest
mkFBMessageTextQ' quickreplies mtyp msg = mkMessageText quickreplies mtyp msg . FBRequestRecipientPhone

mkFBAttachmentImageQ' :: [(Text,Text)] -> Maybe FBRequestNotificationType -> Url -> RecipientPhone -> FBSendRequest
mkFBAttachmentImageQ' quickreplies mtyp url = mkAttachment quickreplies IMAGE mtyp url . FBRequestRecipientPhone

mkFBAttachmentAudioQ' :: [(Text,Text)] -> Maybe FBRequestNotificationType -> Url -> RecipientPhone -> FBSendRequest
mkFBAttachmentAudioQ' quickreplies mtyp url = mkAttachment quickreplies AUDIO mtyp url . FBRequestRecipientPhone

mkFBAttachmentVideoQ' :: [(Text,Text)] -> Maybe FBRequestNotificationType -> Url -> RecipientPhone -> FBSendRequest
mkFBAttachmentVideoQ' quickreplies mtyp url = mkAttachment quickreplies VIDEO mtyp url . FBRequestRecipientPhone

mkFBAttachmentFileQ' :: [(Text,Text)] -> Maybe FBRequestNotificationType -> Url -> RecipientPhone -> FBSendRequest
mkFBAttachmentFileQ' quickreplies mtyp url = mkAttachment quickreplies FILE mtyp url . FBRequestRecipientPhone

mkFBGenericTemplateQ' :: [(Text,Text)] -> Maybe FBRequestNotificationType -> [FBRequestGenericTemplateElement] -> RecipientPhone -> FBSendRequest
mkFBGenericTemplateQ' quickreplies mtyp elems = mkGenericTemplate quickreplies mtyp elems . FBRequestRecipientPhone

mkFBButtonTemplateQ' :: [(Text,Text)] -> Message -> Maybe FBRequestNotificationType -> [FBRequestTemplateButton] -> RecipientPhone -> FBSendRequest
mkFBButtonTemplateQ' quickreplies msg mtyp buttons = mkButtonTemplate quickreplies msg mtyp buttons . FBRequestRecipientPhone
