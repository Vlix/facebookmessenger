module Web.Facebook.Messenger.Types 
    ( FacebookCallbackHandlers (..)
    , module Web.Facebook.Messenger.Types.Responses
    , module Web.Facebook.Messenger.Types.Callbacks
    , module Web.Facebook.Messenger.Types.Requests
    , module Web.Facebook.Messenger.Types.Static
    , SenderID
    , RecipientID
    , RecipientPhone
    , Timestamp
    , MID
    , Sequence
    , Url
    , Message
    ) where

import Data.Text

import Web.Facebook.Messenger.Types.Callbacks
import Web.Facebook.Messenger.Types.Requests
import Web.Facebook.Messenger.Types.Responses
import Web.Facebook.Messenger.Types.Static

type SenderID       = Text
type RecipientID    = Text
type RecipientPhone = Text
type Timestamp      = Int
type MID            = Text
type Sequence       = Int
type Url            = Text
type Message        = Text

data FacebookCallbackHandlers a = FacebookCallbackHandlers
    { fb_messageHandler     :: SenderID -> RecipientID -> Timestamp -> MID -> Sequence -> Text -> Maybe FBCallbackQuickReply -> a
    , fb_attachmentHandler  :: SenderID -> RecipientID -> Timestamp -> MID -> Sequence -> [FBCallbackAttachment] -> a
    , fb_postbackHandler    :: SenderID -> RecipientID -> Timestamp -> Text -> a
    , fb_authHandler        :: SenderID -> RecipientID -> Timestamp -> Text -> a
    , fb_deliveryHandler    :: SenderID -> RecipientID -> FBCallbackDelivery -> a
    , fb_accountLinkHandler :: SenderID -> RecipientID -> Timestamp -> FBCallbackAccountLink -> a
    , fb_readHandler        :: SenderID -> RecipientID -> Timestamp -> FBCallbackRead -> a
    , fb_echoHandler        :: SenderID -> RecipientID -> Timestamp -> FBCallbackEcho -> a
    }