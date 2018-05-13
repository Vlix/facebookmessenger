{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment.Templates
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Template attachments to be sent to the user to give a richer experience
-}
module Web.Facebook.Messenger.Types.Requests.Attachment.Templates (
  -- * Button Template
  --
  -- | Use the "ButtonTemplate" with the Send API to send a text and buttons attached to request input from the user.
  -- The buttons can open a URL, or make a back-end call to your webhook, start a phone call, etc.
  buttonTemplateP
  -- * Generic Template
  --
  -- | Use the "GenericTemplate" with the Send API to send a horizontal scrollable carousel of items,
  -- each composed of an image attachment, short description and buttons to request input from the user.
  --
  -- Buttons in generic templates can do the following:
  --
  -- * open a URL
  -- * make a postback to your webhook
  -- * call a phone number
  -- * open a share dialog
  -- * open a payment dialog
  -- * For all the things you can do, see `TemplateButton`
  , genericTemplateP
  , genericTemplateP_
  -- * List Template
  --
  -- | Use the "ListTemplate" with the Send API to send a vertical list of up to 4 items.
  --
  -- The style of the first item is controlled by `ListStyle`.
  -- The value can be `ListLARGE` or `ListCOMPACT`. To send a list view as a plain list (with no cover item),
  -- set the `ListStyle` to `ListCOMPACT`; otherwise, the first element will be rendered
  -- as the cover item and the image_url is required for the first element.
  --
  -- Please take note of the following limitations:
  --
  -- * You may send at least 2 elements and at most 4 elements.
  -- * Adding a button to each element is optional. You may only have up to 1 button per element.
  -- * You may have up to 1 global button.
  , listTemplateP
  , listTemplateP_
  -- * Media Template
  --
  -- The media template allows you to send images, GIFs, and video as a structured message with an optional button.
  -- Videos and animated GIFs sent with the media template are playable in the conversation.
  --
  -- The media template can be sent via the Send API and from the Messenger webview
  -- with the Messenger Extension SDK's @beginShareFlow()@ function.
  , mediaTemplateP
  -- * Open Graph Template
  --
  -- | Use "OpenGraphTemplate" with the Send API to send a structured music template.
  --
  -- The information used in this template is gathered from meta data on the site located at the provided URL in the template.
  , openGraphTemplateP
  -- * Template Payload
  , TemplatePayload (..)
  -- * Exported modules
  -- ** Regular
  , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.ButtonTemplate
  , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.GenericTemplate
  , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.ListTemplate
  , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.MediaTemplate
  , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.OpenGraphTemplate
  , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.ReceiptTemplate
  -- ** Airline
  , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineBoardingPass
  , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineCheckin
  , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineFlightUpdate
  , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineItinerary
  -- ** Helper modules
  , module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.Airline
  , module Web.Facebook.Messenger.Types.Requests.Extra
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (..), withObject)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.Airline
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineBoardingPass
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineCheckin
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineFlightUpdate
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.AirlineItinerary
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.ButtonTemplate
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.GenericTemplate
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.ListTemplate
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.MediaTemplate
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.OpenGraphTemplate
import Web.Facebook.Messenger.Types.Requests.Attachment.Templates.ReceiptTemplate
import Web.Facebook.Messenger.Types.Requests.Extra
import Web.Facebook.Messenger.Types.Static


-- | Constructor for a Button `TemplatePayload` ("ButtonTemplate")
buttonTemplateP :: Text -- ^ /UTF-8-encoded text of up to 640 characters that appears above the buttons/
                -> NonEmpty TemplateButton -- ^ /Set of 1-3 buttons that appear as call-to-actions/
                -> TemplatePayload
buttonTemplateP title = TButton . ButtonTemplate title


-- | Constructor for a Generic `TemplatePayload`
genericTemplateP :: Bool
                 -- ^ /Set to `False` to disable the native share button in Messenger for the template message./
                 -- /(Though I think the default is False)/
                 -> ImageAspectRatioType
                 -- ^ /Aspect ratio used to render images specified by image_url in element objects./
                 -- /Must be `HORIZONTAL` or `SQUARE`. Default is `HORIZONTAL`./
                 -> NonEmpty GenericElement -- ^ /Data for each bubble in message (Limited to 10)/
                 -> TemplatePayload
genericTemplateP share aspectRatio = TGeneric . GenericTemplate share aspectRatio

-- | Shortcut for a default Generic `TemplatePayload`
--
-- @genericTemplateP_ = genericTemplateP True HORIZONTAL@
genericTemplateP_ :: NonEmpty GenericElement -> TemplatePayload
genericTemplateP_ = genericTemplateP True HORIZONTAL


-- | Constructor for a List `TemplatePayload`
listTemplateP :: ListStyle -> NonEmpty ListElement -> Maybe TemplateButton -> TemplatePayload
listTemplateP style es = TList . ListTemplate style es

-- | Shortcut for a simple default List `TemplatePayload`
listTemplateP_ :: NonEmpty ListElement -> TemplatePayload
listTemplateP_ es = TList $ ListTemplate ListLARGE es Nothing

-- | Constructor for a Media 'TemplatePayload'
mediaTemplateP :: MediaElement -> TemplatePayload
mediaTemplateP = TMedia . MediaTemplate

-- | Constructor for an Open Graph `TemplatePayload`
openGraphTemplateP :: URL -> [TemplateButton] -> TemplatePayload
openGraphTemplateP url = TGraph . OpenGraphTemplate . OpenGraphElement url

-- ------------------ --
--  TEMPLATE REQUEST  --
-- ------------------ --

-- | Data type for all different Template attachments to be sent to FB
data TemplatePayload = TGeneric GenericTemplate
                     | TButton ButtonTemplate
                     | TList ListTemplate
                     | TMedia MediaTemplate
                     | TGraph OpenGraphTemplate
                     | TReceipt ReceiptTemplate
                     | TBoardingPass AirlineBoardingPass
                     | TItinerary AirlineItinerary
                     | TCheckin AirlineCheckin
                     | TFlightUpdate AirlineFlightUpdate
  deriving (Eq, Show, Read, Ord)

instance ToJSON TemplatePayload where
  toJSON (TGeneric x) = toJSON x
  toJSON (TButton x) = toJSON x
  toJSON (TList x) = toJSON x
  toJSON (TMedia x) = toJSON x
  toJSON (TGraph x) = toJSON x
  toJSON (TReceipt x) = toJSON x
  toJSON (TBoardingPass x) = toJSON x
  toJSON (TItinerary x) = toJSON x
  toJSON (TCheckin x) = toJSON x
  toJSON (TFlightUpdate x) = toJSON x

instance FromJSON TemplatePayload where
  parseJSON = withObject "TemplatePayload" $ \o ->
        TGeneric <$> parseJSON (Object o)
    <|> TButton <$> parseJSON (Object o)
    <|> TList <$> parseJSON (Object o)
    <|> TMedia <$> parseJSON (Object o)
    <|> TGraph <$> parseJSON (Object o)
    <|> TReceipt <$> parseJSON (Object o)
    <|> TBoardingPass <$> parseJSON (Object o)
    <|> TItinerary <$> parseJSON (Object o)
    <|> TCheckin <$> parseJSON (Object o)
    <|> TFlightUpdate <$> parseJSON (Object o)
