{-|
Module      : Web.Facebook.Messenger.Types.Requests.Extra
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Some templates as well as the persistent menu support buttons that can perform different kinds of actions:

* @------------------ [`URLButton`]@ Can be used to open a webpage in the in-app browser.
* @------------- [`PostbackButton`]@ Sends back developer-defined payload so you can perform an action or reply back.
* @----------------- [`CallButton`]@ Dials a phone number when tapped.
* @---------------- [`ShareButton`]@ Opens a share dialog in Messenger enabling people to share message bubbles with friends.
* @------------------ [`BuyButton`]@ Opens a checkout dialog to enables purchases.
* @[`LogInButton` and `LogOutButton`]@ Used in Account Linking flow intended to deliver page-scoped user id on web safely.
-}
module Web.Facebook.Messenger.Types.Requests.Extra (
  -- * Template Buttons
  TemplateButton (..)
  -- ** URL Button
  --
  -- | The URL Button can be used to open a web page in the in-app browser.
  -- This button can be used with the Button, Generic and List Templates.
  --
  --
  -- Linking to your Bot from a URL Button:
  -- If you're specifying a URL button as part of a custom message to be shared in the share button or inside the webview,
  -- you may wish for the recipient of the share to enter your bot using the button (rather than a URL in the webview).
  --
  -- To enable, all you must do is link to the bot using m.me. If you want the recipient of the share
  -- to activate a specific flow when they click the link, consider adding a ref parameter.
  --
  -- For instance:
  --
  -- > ...
  -- > "buttons":[
  -- >     {
  -- >       "type":"web_url",
  -- >       "url":"https://m.me/petershats?ref=take_quiz",
  -- >       "title":"Take the Hat Quiz",
  -- >     }
  -- >   ]
  -- > ...
  --
  -- When the user taps the button, the bot will directly open, bypassing the webview.
  , urlButton
  , urlButton_
  , urlButtonME
  , URLButton (..)
  -- ** Postback Button
  --
  -- | When this is tapped, we will send a call to the postback webhook.
  -- This is useful when you want to invoke an action in your bot.
  -- You can attach a metadata payload to the button that will be sent back to your webhook.
  --
  -- This button can be used with the Button, Generic and List Templates.
  --
  -- It is a good practice to send a message back to acknowledge when a postback has been received.
  --
  -- https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_postbacks
  , postbackButton
  , PostbackButton (..)
  -- ** Call Button
  --
  -- | The Call Button can be used to initiate a phone call. This button can be used with the Button and Generic Templates.
  , callButton
  , CallButton (..)
  -- ** Log In/Out Button
  --
  -- | The `LogInButton` begins the account linking flow.
  --
  -- The `LogOutButton` is a trigger for the account unlinking flow.
  --
  -- https://developers.facebook.com/docs/messenger-platform/identity/account-linking
  --
  -- *** Login Button
  , loginButton
  , LogInButton (..)
  -- *** Logout Button
  , logoutButton
  , LogOutButton (..)
  -- ** Share Button
  --
  -- | The Share Button enables people to share your content in Messenger.
  --
  -- @N.B.@ You may also wish to specify contents different than the message the "Share" button is attached to.
  -- For instance, you might want to send an invitation, challenge, or other custom flow for the recipient to see.
  -- To do this, set the optional `ShareContents` attribute.
  -- Note that although the format for `ShareContents` is identical to the `GenericElement`s,
  -- we only support sharing generic templates with a maximum of 1 URL button.
  --
  -- The button you add to `ShareContents` may use the webview to show a page, or use m.me to deep-link to your bot.
  --
  -- /Caveats/:
  --
  -- * Only individual message bubbles can be shared.
  -- * The `ShareButton` can only be sent inside the `GenericTemplate`.
  -- * If your message bubble has a `URLButton` that uses Messenger Extensions (without a fallback URL),
  -- then the behavior of postback and buy buttons will change.
  -- When the recipient taps on one of these buttons, they will start a new thread with the bot.
  -- * In the case of a URL Button using Messenger Extensions, the fallback URL will be opened if specified.
  -- Share, URL, and Phone Number buttons will behave normally.
  , shareButton
  , ShareButton (..)
  , ShareContents (..)
  -- ** Buy Button
  --
  -- | The `BuyButton` enables you to build a checkout experience in Messenger.
  -- This button opens a native checkout dialog in Messenger and enables people to use their information stored in Messenger.
  -- When adding a `BuyButton` to the `GenericTemplate`, two things are rendered on the message bubble:
  -- the total price and a button that opens the dialog. Currently, this only works with the `GenericTemplate`.
  --
  -- This dialog displays the item, a price list, and payment method.
  -- Optionally, you can also request other information such as the person's name, shipping address, phone number and email address.
  --
  -- When the person taps the Pay button in the dialog, we will send you the tokenized credentials
  -- so that you can charge the person for the item. You should confirm the purchase with a message (e.g., with the `ReceiptTemplate`).
  --
  -- /Fixed vs Flexible Pricing/:
  --
  -- The Buy Button supports fixed and flexible pricing. Flexible pricing can be used when you modify pricing based on shipping.
  -- When flexible pricing is declared, the Checkout dialog will render a button that the person can tap to choose the shipping method.
  -- We call your webhook to get information about the shipping names and prices.
  , buyButton
  , buyButton_
  , BuyButton (..)
  , PriceObject (..)
  -- * Generic Element
  , GenericElement (..)
  -- * Default Action
  , defaultAction
  , defaultActionME
  , DefaultAction (..)
  , TemplateAddress (..)
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)

import Web.Facebook.Messenger.Types.Static


-- | Constructor for the URL `TemplateButton`
urlButton :: Text -- ^ /Button title. 20 character limit./
          -> URL
          -- ^ /This URL is opened in a mobile browser when the button is tapped./
          -- /Must use HTTPS protocol if `MessengerExtensions is `True`./
          -> WebviewHeightRatioType -- ^ /Height of the Webview. Valid values: `COMPACT`, `TALL`, `FULL`. Default is `FULL`/
          -> Bool -- ^ /Must be `True` if using Messenger Extensions. Default is `False`/
          -> Maybe URL
          -- ^ /The URL to use on clients that don't support Messenger Extensions./
          -- /If this is not defined, the url will be used as the fallback./
          -- /It may only be specified if `ubMessengerExtensions` is `True`./
          -> WebviewShareType
          -- ^ /Set to `HIDE` to disable the share button in the Webview (for sensitive info)./
          -- /This does not affect any shares initiated by the developer using Extensions./
          -- /Default is `SHOW`/
          -> TemplateButton
urlButton title url heightRatio msgExt fallbackUrl =
    TUrl . URLButton title url heightRatio msgExt fallbackUrl

-- | Shortcut function for a normal `web_url` button with the defaults
--
-- @urlButton_ title url = urlButton title url FULL False Nothing SHOW@
urlButton_ :: Text -> URL -> TemplateButton
urlButton_ title url = urlButton title url FULL False Nothing SHOW

-- | URL button with Messenger Extensions activated.
--
-- @(URL,URL)@ are the URL using Messenger Extensions and the fallback URL
-- in case Messenger Extensions aren't supported on user's side.
urlButtonME :: Text -- ^/ Button title. 20 character limit./
            -> (URL,URL) -- ^/ (URL with Msgr Exts, Fallback URL w/o Msgr Exts)/
            -> WebviewHeightRatioType -- ^/ `FULL` | `TALL` | `COMPACT` (Default is `FULL`)/
            -> WebviewShareType -- ^/ `HIDE` | `SHOW` (Default is `Show`)/
            -> TemplateButton
urlButtonME title (url,fallbackUrl) heightRatio =
    urlButton title url heightRatio True (Just fallbackUrl)


-- | Constructor for the Postback `TemplateButton`
--
-- Button used in Templates. Sends a callback to your server to act on it.
postbackButton :: Text -- ^/ Button title. 20 character limit./
               -> Text -- ^/ This data will be sent back to your webhook. 1000 character limit./
               -> TemplateButton
postbackButton = (TPostback .) . PostbackButton


-- | Constructor for the Call/Phone `TemplateButton`
--
-- Button used in Templates. Direct the user to a phone number to be handled by user's device.
callButton :: Text -- ^/ Button title, 20 character limit./
           -> Text -- ^/ Format must have "+" prefix followed by the country code, area code and local number. For example, +16505551234./
           -> TemplateButton
callButton = (TCall .) . CallButton


-- | Constructor for the Share `TemplateButton`
--
-- Button used in Templates. Makes the template sharable to other users in the user's friend list.
-- @[`GenericElement`]@ are elements that will be shared instead of the template the button is on,
-- in case you don't want the template to be shared, but maybe a general explanation of/invitation to your bot/channel.
shareButton :: [GenericElement] -> TemplateButton
shareButton [] = TShare $ ShareButton Nothing
shareButton es = TShare . ShareButton . Just $ ShareContents es

-- * LogIn Button
--
-- | Button used in Templates. Directs the user to an authentication URL where the user can log in
-- on an external service to (temporarily) link the user to a user of that external service.

-- | Constructor for the Log-in `TemplateButton`
loginButton :: URL -- ^/ Authentication callback URL. Must use HTTPS protocol./
            -> TemplateButton
loginButton = TLogIn . LogInButton

-- * LogIn Button
--
-- | Button used in Templates. Disconnects the link of the user to the logged in external service.

-- | Constructor for the Log-out `TemplateButton`
logoutButton :: TemplateButton
logoutButton = TLogOut LogOutButton

-- * Buy Button
--
-- | Button used in Templates. Directs the user to the payment flow of Facebook Messenger.

-- | Constructor for the Buy `TemplateButton`
buyButton :: Text -- ^/ Developer defined metadata about the purchase./
          -> Text -- ^/ Currency for price. Must be a three digit ISO-4217-3 code. https://developers.facebook.com/docs/payments/reference/supportedcurrencies/
          -> PaymentType -- ^/ Must be `FIXED_AMOUNT` or `FLEXIBLE_AMOUNT`./
          -> Text -- ^/ Name of merchant./
          -> [RequestedUserInfoType]
          -- ^/ Information requested from person that will render in the dialog./
          -- /Valid values: `SHIPPING_ADDRESS`, `CONTACT_NAME`, `CONTACT_PHONE`, `CONTACT_EMAIL`./
          -- /You can config these based on your product need./
          -> [PriceObject] -- ^ /List of objects used to calculate total price. Each label is rendered as a line item in the checkout dialog./
          -> TemplateButton
buyButton payload currency payType name rui =
    TBuy . BuyButton payload currency False payType name rui

-- | Constructor for the Buy `TemplateButton` (as a test payment, so no actual purchases will be made)
buyButton_ :: Text -> Text -> PaymentType -> Text -> [RequestedUserInfoType] -> [PriceObject] -> TemplateButton
buyButton_ payload currency payType name rui =
    TBuy . BuyButton payload currency True payType name rui


-- * Default Action
--
-- | This works the same as a `URLButton`, but makes a Template bubble clickable,
-- instead of just a button below one.

-- | Constructor for a `DefaultAction` (everything defaulted except the URL)
defaultAction :: URL -> DefaultAction
defaultAction url = DefaultAction url FULL False Nothing SHOW

-- | Constructor for a `DefaultAction` using Messenger Extensions
defaultActionME :: (URL,URL) -- ^ /(URL with Msgr Exts, Fallback URL w/o Msgr Exts)/
                -> WebviewHeightRatioType -- ^ /`FULL` / `TALL` / `COMPACT` (Default is `FULL`)/
                -> WebviewShareType -- ^ /`HIDE` / `SHOW` (Default is `Show`)/
                -> DefaultAction
defaultActionME (url,fallbackUrl) heightRatio =
    DefaultAction url heightRatio True $ Just fallbackUrl


-- ------------------ --
--  TEMPLATE BUTTONS  --
-- ------------------ --

-- | The types of buttons that are used in most templates.
data TemplateButton = TUrl URLButton
                    | TPostback PostbackButton
                    | TCall CallButton
                    | TLogIn LogInButton
                    | TLogOut LogOutButton
                    | TShare ShareButton
                    | TBuy BuyButton
  deriving (Eq, Show, Read, Ord)

instance ToJSON TemplateButton where
  toJSON (TUrl x) = toJSON x
  toJSON (TPostback x) = toJSON x
  toJSON (TCall x) = toJSON x
  toJSON (TLogIn x) = toJSON x
  toJSON (TLogOut x) = toJSON x
  toJSON (TShare x) = toJSON x
  toJSON (TBuy x) = toJSON x

instance FromJSON TemplateButton where
  parseJSON = withObject "TemplateButton" $ \o ->
        TUrl <$> parseJSON (Object o)
    <|> TPostback <$> parseJSON (Object o)
    <|> TCall <$> parseJSON (Object o)
    <|> TLogIn <$> parseJSON (Object o)
    <|> TLogOut <$> parseJSON (Object o)
    <|> TShare <$> parseJSON (Object o)
    <|> TBuy <$> parseJSON (Object o)

-- | A button to a website
data URLButton = URLButton
    { ubTitle :: Text
    -- ^ 20 char limit (30 char limit when used in Persistent Menu)
    , ubUrl :: URL
    -- ^ This URL is opened in a mobile browser when the button is tapped.
    -- Must use HTTPS protocol if `ubMessengerExtensions` is `True`.
    , ubWebviewHeightRatio :: WebviewHeightRatioType
    -- ^ Height of the Webview. Valid values: `COMPACT`, `TALL`, `FULL`. Default is `FULL`
    , ubMessengerExtensions :: Bool
    -- ^ Must be `True` if using Messenger Extensions. Default is `False`
    , ubFallbackUrl :: Maybe URL
    -- ^ The URL to use on clients that don't support Messenger Extensions.
    -- If this is not defined, the url will be used as the fallback.
    -- It may only be specified if `ubMessengerExtensions` is `True`.
    , ubWebviewShareButton :: WebviewShareType
    -- ^ Set to `HIDE` to disable the share button in the Webview (for sensitive info).
    -- This does not affect any shares initiated by the developer using Extensions.
    -- Default is `SHOW`
    } deriving (Eq, Show, Read, Ord)

instance ToJSON URLButton where
  toJSON (URLButton title url wvratio extensions fallback share) =
      object' [ "type" .=! String "web_url"
              , "title" .=! title
              , "url" .=! url
              , mDefault "webview_height_ratio" FULL wvratio
              , mDefault "messenger_extensions" False extensions
              , "fallback_url" .=!! fallback
              , mDefault "webview_share_button" SHOW share
              ]

instance FromJSON URLButton where
  parseJSON = checkValue
      "URLButton"
      "type"
      ("web_url" :: Text)
      $ \o -> do
          share <- o .:? "webview_share_button" :: Parser (Maybe WebviewShareType)
          URLButton <$> o .: "title"
                    <*> o .: "url"
                    <*> o .:? "webview_height_ratio" .!= FULL
                    <*> o .:? "messenger_extensions" .!= False
                    <*> o .:? "fallback_url"
                    <*> pure (fromMaybe SHOW share)

-- | A button that sends a callback to your bot/server
data PostbackButton = PostbackButton
    { pbbTitle :: Text -- ^ 20 char limit (30 char limit when used in Persistent Menu)
    , pbbPayload :: Text -- ^ This data will be sent back to your webhook. 1000 character limit.
    } deriving (Eq, Show, Read, Ord)

instance ToJSON PostbackButton where
  toJSON (PostbackButton title payload) =
      object [ "type" .= String "postback"
             , "title" .= title
             , "payload" .= payload
             ]

instance FromJSON PostbackButton where
  parseJSON = checkValue
      "PostbackButton"
      "type"
      ("postback" :: Text)
      $ \o -> PostbackButton <$> o .: "title"
                             <*> o .: "payload"

-- | A button which starts a phone call
data CallButton = CallButton
    { cbTitle :: Text -- ^ Button title, 20 character limit.
    , cbPayload :: Text -- ^ Format must have "+" prefix followed by the country code, area code and local number. For example, +16505551234.
    } deriving (Eq, Show, Read, Ord)

instance ToJSON CallButton where
  toJSON (CallButton title payload) =
      object [ "type" .= String "phone_number"
             , "title" .= title
             , "payload" .= payload
             ]

instance FromJSON CallButton where
  parseJSON = checkValue
      "CallButton"
      "type"
      ("phone_number" :: Text)
      $ \o -> CallButton <$> o .: "title"
                             <*> o .: "payload"

-- | A button which lets the user share the template that contains the share button
-- or a different custom template defined in the `ShareContents`
newtype ShareButton = ShareButton { shareContents :: Maybe ShareContents }
  deriving (Eq, Show, Read, Ord)

instance FromJSON ShareButton where
  parseJSON = checkValue
      "ShareButton"
      "type"
      ("element_share" :: Text)
      $ \o -> ShareButton <$> o .:? "share_contents"

instance ToJSON ShareButton where
  toJSON (ShareButton msc) =
      object' [ "type" .=! String "element_share"
              , "share_contents" .=!! msc
              ]

-- | The message that you wish the recipient of the share to see,
-- if it is different from the one this button is attached to.
-- The format follows that used in Send API, but must be a generic template with up to one URL button.
newtype ShareContents =
      ShareContents { elements :: [GenericElement] }
  deriving (Eq, Show, Read, Ord)

instance FromJSON ShareContents where
  parseJSON = withObject "ShareContents" $ \o -> do
      att <- o .: "attachment"
      typ <- att .: "type" :: Parser Text
      unless (typ == "template") $
        fail $ "ShareContents: wrong \"type\" value: " `mappend` unpack typ
      pl <- att .: "payload"
      tempType <- pl .: "template_type" :: Parser Text
      unless (tempType == "generic") $
        fail $ "ShareContents: wrong \"template_type\" value: " `mappend` unpack tempType
      ShareContents <$> pl .: "elements"

instance ToJSON ShareContents where
  toJSON (ShareContents elems) = obj
    where obj = object ["attachment" .= attObj]
          attObj = object [ "type" .= String "template"
                          , "payload" .= plObj
                          ]
          plObj = object [ "template_type" .= String "generic"
                         , "elements" .= elems
                         ]

-- | `URL` should be the authentication callback URL. Must use HTTPS protocol.
newtype LogInButton = LogInButton { accountLink :: URL }
  deriving (Eq, Show, Read, Ord)

instance FromJSON LogInButton where
  parseJSON = checkValue
      "LogInButton"
      "type"
      ("account_link" :: Text)
      $ \o -> LogInButton <$> o .: "url"

instance ToJSON LogInButton where
  toJSON (LogInButton link) =
      object [ "type" .= String "account_link"
             , "url" .= link
             ]

-- | A button to let the user unlink from the linked account
data LogOutButton = LogOutButton
  deriving (Eq, Show, Read, Ord)

instance FromJSON LogOutButton where
  parseJSON = checkValue
      "LogOutButton"
      "type"
      ("account_unlink" :: Text)
      $ \_ -> pure LogOutButton

instance ToJSON LogOutButton where
  toJSON LogOutButton =
      object ["type" .= String "account_unlink"]

-- | A button which starts the payment sequence
data BuyButton = BuyButton
    { bbPayload :: Text -- ^ Developer defined metadata about the purchase.
    , bbCurrency :: Text -- ^ Currency for price.
    , bbIsTestPayment :: Bool -- ^ Whether this is a test payment. Once set to true, the charge will be a dummy charge.
    , bbPaymentType :: PaymentType -- ^ Must be `FIXED_AMOUNT` or `FLEXIBLE_AMOUNT`.
    , bbMerchantName :: Text -- ^ Name of merchant.
    , bbRequestedUserInfo :: [RequestedUserInfoType]
    -- ^ Information requested from person that will render in the dialog.
    -- Valid values: `SHIPPING_ADDRESS`, `CONTACT_NAME`, `CONTACT_PHONE`, `CONTACT_EMAIL`.
    -- You can config these based on your product need.
    , bbPriceList :: [PriceObject] -- ^ List of objects used to calculate total price. Each label is rendered as a line item in the checkout dialog.
    } deriving (Eq, Show, Read, Ord)

instance FromJSON BuyButton where
  parseJSON = checkValue
      "BuyButton"
      "type"
      ("payment" :: Text)
      $ \o -> do
        ob <- o .: "payment_summary" :: Parser Object
        BuyButton <$> o .: "payload"
                  <*> ob .: "currency"
                  <*> ob .:? "is_test_payment" .!= False
                  <*> ob .: "payment_type"
                  <*> ob .: "merchant_name"
                  <*> ob .: "requested_user_info"
                  <*> ob .: "price_list"

instance ToJSON BuyButton where
  toJSON bb =
      object [ "type" .= String "payment"
             , "title" .= String "buy"
             , "payload" .= bbPayload bb
             , "payment_summary" .= summary
             ]
    where summary = object' [ "currency" .=! bbCurrency bb
                            , mDefault "is_test_payment" False $ bbIsTestPayment bb
                            , "payment_type" .=! bbPaymentType bb
                            , "merchant_name" .=! bbMerchantName bb
                            , "requested_user_info" .=! bbRequestedUserInfo bb
                            , "price_list" .=! bbPriceList bb
                            ]

-- | Item in a list of things that comprise the purchase.
data PriceObject = PriceObject { prLabel :: Text -- ^ Label for line item.
                               , prAmount :: Text -- ^ Amount of line item.
                               }
  deriving (Eq, Show, Read, Ord)

instance FromJSON PriceObject where
  parseJSON = withObject "PriceObject" $ \o ->
    PriceObject <$> o .: "label"
                <*> o .: "amount"

instance ToJSON PriceObject where
  toJSON (PriceObject label amount) =
    object [ "label" .= label
           , "amount" .= amount
           ]


-- ----------------- --
--  GENERIC ELEMENT  --
-- ----------------- --

-- | The `GenericElement` type and instances are in this module because
-- the ShareButton depends on it, and the `GenericElement` depends on `TemplateButton`
data GenericElement = GenericElement
    { geTitle :: Text -- ^ Bubble title (80 char limit)
    , geSubtitle :: Maybe Text -- ^ Bubble subtitle (80 char limit)
    , geImageUrl :: Maybe URL -- ^ Bubble image (1.91:1 or 1:1 image ratio, depending on the `geImageAspectRatio`)
    , geDefaultAction :: Maybe DefaultAction -- ^ Default action to be triggered when user taps on the element
    , geBuyButton :: Maybe BuyButton
    , geButtons :: [TemplateButton]
    -- ^ Set of buttons that appear as call-to-actions (3 button limit)
    -- 2 button limit if `geBuyButton` is @Just BuyButton{}@
    }
  deriving (Eq, Show, Read, Ord)

instance FromJSON GenericElement where
  parseJSON = withObject "GenericElement" $ \o -> do
          btns <- o .:? "buttons" .!= []
          let (mBuy,restBtns) = getBtns btns
          GenericElement <$> o .: "title"
                         <*> o .:? "subtitle"
                         <*> o .:? "image_url"
                         <*> o .:? "default_action"
                         <*> pure mBuy
                         <*> pure (take 3 restBtns)
    where getBtns [] = (Nothing,[])
          getBtns (TBuy bb:rest) = (Just bb,rest)
          getBtns btns = (Nothing,btns)

instance ToJSON GenericElement where
  toJSON (GenericElement title subtitle image da buy buttons) =
      object' [ "title" .=! title
              , "subtitle" .=!! subtitle
              , "image_url" .=!! image
              , "default_action" .=!! da
              , mEmptyList "buttons" $ take 3 $ btnList buy
              ]
    where btnList Nothing = buttons
          btnList (Just btn) = TBuy btn : buttons


-- ---------------- --
--  DEFAULT ACTION  --
-- ---------------- --

-- | Using this, you can enable people to open a URL when bubble of a /`GenericTemplate`/ or the row of a /`ListTemplate`/ is tapped.
data DefaultAction = DefaultAction
    { daUrl :: URL -- ^ This URL is opened in a mobile browser when the template is tapped
    , daWebviewHeightRatio :: WebviewHeightRatioType -- ^ Height of the Webview. Default is `FULL`.
    , daMessengerExtensions :: Bool -- ^ Must be `True` if using Messenger Extensions. Default is `False`
    , daFallback :: Maybe URL -- ^ URL to use on clients that don't support Messenger Extensions. If this is not defined, the url will be used as the fallback.
    , daWebviewShareButton :: WebviewShareType -- ^ Show the share button on the webview. Default is `SHOW`.
    } deriving (Eq, Show, Read, Ord)

instance ToJSON DefaultAction where
  toJSON (DefaultAction url webview ext fallback share) =
    object' [ "type" .=! String "web_url"
            , "url" .=! url
            , mDefault "webview_height_ratio" FULL webview
            , mDefault "messenger_extensions" False ext
            , "fallback_url" .=!! fallback
            , mDefault "webview_share_button" SHOW share
            ]

instance FromJSON DefaultAction where
  parseJSON = checkValue
      "DefaultAction"
      "type"
      ("web_url" :: Text)
      $ \o -> DefaultAction <$> o .: "url"
                            <*> o .:? "webview_height_ratio" .!= FULL
                            <*> o .:? "messenger_extensions" .!= False
                            <*> o .:? "fallback_url"
                            <*> o .:? "webview_share_button" .!= SHOW


-- ------------------ --
--  TEMPLATE ADDRESS  --
-- ------------------ --

-- | Address of a user
data TemplateAddress = TemplateAddress
  { taStreet1 :: Text -- ^ Street address, line 1
  , taStreet2 :: Maybe Text -- ^ Street address, line 2
  , taCity :: Text -- ^ City name
  , taPostalCode :: Text -- ^ Postal code
  , taState :: Text -- ^ State abbreviation. Can be a region or province for non-US addresses
  , taCountry :: Text -- ^ Two-letter (/ISO 3166-1 alpha-2/) country abbreviation
  } deriving (Eq, Show, Read, Ord)

instance ToJSON TemplateAddress where
  toJSON (TemplateAddress street1 street2 city postcode state country) =
      object' [ "street_1" .=! street1
              , "street_2" .=!! street2
              , "city" .=! city
              , "postal_code" .=! postcode
              , "state" .=! state
              , "country" .=! country
              ]

instance FromJSON TemplateAddress where
  parseJSON = withObject "TemplateAddress" $ \o ->
      TemplateAddress <$> o .: "street_1"
                     <*> o .:? "street_2"
                     <*> o .: "city"
                     <*> o .: "postal_code"
                     <*> o .: "state"
                     <*> o .: "country"
