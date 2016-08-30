module Web.Facebook.Messenger.Types 
    ( CallbackHandlers (..)
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

data CallbackHandlers a = CallbackHandlers
    { messageHandler     :: SenderID -> RecipientID -> Timestamp -> MID -> Sequence -> Text -> Maybe CallbackQuickReply -> a
    , attachmentHandler  :: SenderID -> RecipientID -> Timestamp -> MID -> Sequence -> [CallbackAttachment] -> a
    , postbackHandler    :: SenderID -> RecipientID -> Timestamp -> Text -> a
    , authHandler        :: SenderID -> RecipientID -> Timestamp -> Text -> a
    , deliveryHandler    :: SenderID -> RecipientID -> Delivery -> a
    , accountLinkHandler :: SenderID -> RecipientID -> Timestamp -> AccountLink -> a
    , readHandler        :: SenderID -> RecipientID -> Timestamp -> ReadCallback -> a
    , echoHandler        :: SenderID -> RecipientID -> Timestamp -> Echo -> a
    }