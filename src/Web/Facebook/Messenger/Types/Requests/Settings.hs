{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Web.Facebook.Messenger.Types.Requests.Settings
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This module provides a way to construct a compounded request
for the Messenger Profile API. The Messenger Profile for your
app is where you set properties that define various aspects
of the following Messenger Platform features.

https://developers.facebook.com/docs/messenger-platform/reference/messenger-profile-api
-}
module Web.Facebook.Messenger.Types.Requests.Settings (
  -- * Messenger Profile API
  --
  -- | Using 'mconcat' or 'mappend' you can construct a 'ProfileRequest' out of several parts.
  --
  -- In case the same kind of request are appended to each other monoidally,
  -- the result will depend on the type inside the request.
  -- In case the request has a list of things ('greeting', 'persistentMenu' and
  -- 'whiteListedDomains') the lists will be appended to eachother. In every other
  -- case, only one is kept in the actual request. Namely the last one.
  --
  -- @
  -- 'mconcat' ['greeting' ['GreetingSetting' "default" "Hello, how are you doing?"
  --                   ,'GreetingSetting' "en_US" "Hey man, how ya doin'?"]
  --         ,'getStarted' "getstarted"
  --         ,'whiteListedDomains' [\"https:\/\/example.com\/\",\"https:\/\/server.somewhere.else\/\"]
  --         ]
  -- @
  --
  -- N.B. In case you need an empty 'ProfileRequest',
  -- you can use 'mempty'. But it's unlikely you'll need it.

  -- * Profile Request
  ProfileRequest (..)
  -- ** Greeting
  , greeting
  , defaultGreeting
  , Greeting (..)
  , GreetingSetting (..)
  -- ** Get Started
  , getStarted
  , GetStartedButton (..)
  -- ** Persistent Menu
  , persistentMenu
  , PersistentMenu (..)
  , PersistentMenuSetting (..)
  , PersistentMenuItem (..)
  , persistentUrlItem
  , persistentUrlItem_
  , persistentUrlItemME
  , persistentPostbackItem
  , persistentNestedItem
  , PersistentMenuItemNested (..)
  -- ** Whitelisting Domains
  , whiteListedDomains
  , WhiteListedDomains (..)
  -- ** Account Linking
  , accountLinkingUrl
  , AccountLinkingUrl (..)
  -- ** Payment Settings
  , paymentSettings
  , PaymentSettings (..)
  -- ** Target Audience
  , targetAudience
  , TargetAudience (..)
  , TargetCountries (..)
  -- ** Home URL
  , homeUrl
  , homeUrl_
  , HomeUrl (..)
  )
where


import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests.Extra
import Web.Facebook.Messenger.Types.Static


-- | The greeting property of your app's Messenger profile allows you
-- to specify the greeting message people will see on the welcome
-- screen of your app. The welcome screen is displayed for people
-- interacting with your bot for the first time.
--
-- If greeting text is not set for the user's locale, the standard
-- greeting text set with the thread settings API will be shown in
-- the welcome screen. If the standard greeting text is not set
-- either, the page description will be shown.
greeting :: [GreetingSetting] -> ProfileRequest
greeting x = mempty{prGreeting = Just (Greeting x)}

-- | Convenience function to set a default 'GreetingSetting'
defaultGreeting :: Text -> GreetingSetting
defaultGreeting = GreetingSetting Nothing

-- | A bot's welcome screen can display a Get Started button.
-- When this button is tapped, the Messenger Platform will send
-- a (/Callbacks./)Postback callback event to your webhook.
-- Bots that add the button may also wish to configure their greeting text.
--
-- The welcome screen is only shown the first time the user interacts
-- with the Page on Messenger. While the app is in development mode,
-- the welcome screen will only be visible to people with the
-- administrator, developer, and tester app roles.
getStarted :: Text -- ^ 1000 character limit.
           -> ProfileRequest
getStarted x = mempty{prGetStarted = Just (GetStartedButton x)}

-- | The persistent menu can be set for your bot to help people discover
-- and more easily access your functionality throughout the conversation.
--
-- The Persistent Menu is always available to the user. This menu should
-- contain top-level actions that users can enact at any point. Having a
-- persistent menu easily communicates the basic capabilities of your bot
-- for first-time and returning users. The menu will automatically appear
-- in a thread if the person has been away for a certain period of time
-- and returns.
--
-- The Get Started Button must be set to use the Persistent Menu
persistentMenu :: [PersistentMenuSetting] -> ProfileRequest
persistentMenu x = mempty{prPersistentMenu = Just (PersistentMenu x)}

-- | The @whitelisted_domains@ property of your app's Messenger profile specifies
-- a list of third-party domains that are accessible in the Messenger webview
-- for use with the @Messenger Extensions SDK@, and for the @checkbox plugin@.
--
-- @[`Text`]@ is a list of domains being used.
-- All domains must be valid. Up to 50 domains allowed.
whiteListedDomains :: [URL] -> ProfileRequest
whiteListedDomains x = mempty{prWhitelistedDomains = Just (WhiteListedDomains x)}

-- | Messenger's Account Linking allows a secure and consistent
-- way to link user accounts in your app to the user's Messenger
-- account. When a user has linked their account, Log In and
-- Log Out buttons will be shown in the profile screen. Using this
-- feature requires a valid account_linking_url in your app's
-- Messenger Profile.
--
-- More info about account linking [HERE]
-- (https://developers.facebook.com/docs/messenger-platform/identity/account-linking)
accountLinkingUrl :: URL -> ProfileRequest
accountLinkingUrl x = mempty{prAccountLinkingUrl = Just (AccountLinkingUrl x)}

-- | The payment_settings property of your app's Messenger Profile
-- provides the Messenger Platform with several settings needed to
-- implement various aspects of payments in your app:
--
-- * Privacy policy URL: Required for apps that implement `BuyButton` payments.
-- * Public key: Required for apps that implement webview payments or tokenized payment with the `BuyButton`.
-- * Test users: Required to start testing while waiting to be accepted in the Payment beta program.
--
-- More info about payments [HERE]
-- (https://developers.facebook.com/docs/messenger-platform/payments)
paymentSettings :: PaymentSettings -> ProfileRequest
paymentSettings x = mempty{prPaymentSettings = Just x}

-- | 'TargetAudience' allows you to customize the audience
-- that will see your bot in the Discover tab on Messenger.
-- Other users can still find and use your bot through other
-- channels (e.g. search, m.me URL).
--
-- There are three types of Target Audience settings:
--
-- 1. Open to all users,
-- 2. Closed to all users, and
-- 3. Open or closed to custom set of users.
--
-- More info about the Discover Tab [HERE]
-- (https://developers.facebook.com/docs/messenger-platform/discover)
targetAudience :: TargetAudience -> ProfileRequest
targetAudience x = mempty{prTargetAudience = Just x}

-- | This `homeUrl` property of your app's Messenger profile
-- allows your app to enable a Chat Extension in the composer
-- drawer in Messenger. It controls what is displayed when the
-- Chat Extension is invoked via the composer drawer in Messenger.
--
-- More info on Chat Extensions [HERE]
-- (https://developers.facebook.com/docs/messenger-platform/guides/chat-extensions)
--
-- __N.B.__ The URL specified here is for Chat Extensions only.
-- The web app at the URL you specify should use the APIs available
-- in the webview to implement useful functionality for people that use your Chat Extension.
--
-- /This field is not a place to put your bot or company's homepage./
homeUrl :: HomeUrl -> ProfileRequest
homeUrl x = mempty{prHomeUrl = Just x}

-- | Convenience function to just give the 'URL'.
-- Hides the share button (default) and is not in test.
homeUrl_ :: URL -> ProfileRequest
homeUrl_ url = homeUrl (HomeUrl url HIDE False)


-- ---------------------- --
--  SEND SETTING REQUEST  --
-- ---------------------- --

-- | Sets the values of one or more Messenger Profile properties.
-- Only properties set in the request body will be overwritten.
-- To set or update Messenger Profile properties you must have
-- the \'Administrator\' role for the Page associated with the app.
--
-- @https:\/\/graph.facebook.com\/v2.12\/me\/thread_settings?access_token=<PAGE_ACCESS_TOKEN>@
data ProfileRequest = ProfileRequest
    { prGreeting :: Maybe Greeting
    , prGetStarted :: Maybe GetStartedButton
    , prPersistentMenu :: Maybe PersistentMenu
    , prWhitelistedDomains :: Maybe WhiteListedDomains
    , prAccountLinkingUrl :: Maybe AccountLinkingUrl
    , prPaymentSettings :: Maybe PaymentSettings
    , prTargetAudience :: Maybe TargetAudience
    , prHomeUrl :: Maybe HomeUrl
    } deriving stock (Eq, Show, Read, Ord)

-- | Empty `ProfileRequest` used for the monoid instance
emptyProfileRequest :: ProfileRequest
emptyProfileRequest = ProfileRequest
    { prGreeting = Nothing
    , prGetStarted = Nothing
    , prPersistentMenu = Nothing
    , prWhitelistedDomains = Nothing
    , prAccountLinkingUrl = Nothing
    , prPaymentSettings = Nothing
    , prTargetAudience = Nothing
    , prHomeUrl = Nothing
    }

-- | Monoidally build up a `ProfileRequest`
instance Semigroup ProfileRequest where
  p1 <> p2 = ProfileRequest
      { prGreeting = prGreeting p1 <:> prGreeting p2
      , prGetStarted = prGetStarted p1 >:> prGetStarted p2
      , prPersistentMenu = prPersistentMenu p1 <:> prPersistentMenu p2
      , prWhitelistedDomains = prWhitelistedDomains p1 <:> prWhitelistedDomains p2
      , prAccountLinkingUrl = prAccountLinkingUrl p1 >:> prAccountLinkingUrl p2
      , prPaymentSettings = prPaymentSettings p1 >:> prPaymentSettings p2
      , prTargetAudience = prTargetAudience p1 >:> prTargetAudience p2
      , prHomeUrl = prHomeUrl p1 >:> prHomeUrl p2
      }

-- | Small helper function that's terser than just using `Last`
(>:>) :: Maybe a -> Maybe a -> Maybe a
(>:>) x Nothing = x
(>:>) _ x = x

(<:>) :: Semigroup a => Maybe a -> Maybe a -> Maybe a
p <:> Nothing = p
Nothing <:> p = p
Just p1 <:> Just p2 = Just $ p1 <> p2

-- | Monoidally build up a `ProfileRequest`
instance Monoid ProfileRequest where
  mempty = emptyProfileRequest
  mappend = (<>)

-- | Wrapper around [`GreetingSetting`]
newtype Greeting = Greeting { greetingText :: [GreetingSetting] }
  deriving stock (Eq, Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON, Semigroup)

-- | Greeting for a specific locale
--
-- You can personalize the greeting text using the person's name.
-- You can use the following template strings:
--
-- * {{user_first_name}}
-- * {{user_last_name}}
-- * {{user_full_name}}
--
-- https://developers.facebook.com/docs/messenger-platform/messenger-profile/supported-locales
data GreetingSetting = GreetingSetting
    { gsLocale :: Maybe FBLocale
    -- ^ Locale of the greeting text, shown when the person's
    -- locale matches the provided locale. Must be in UTF-8.
    -- 160 character limit. You must at least specify greeting
    -- text for the default locale, which will be displayed if
    -- no provided locale matches the person's locale.
    , gsText :: Text -- ^ The greeting text for the specific locale.
    } deriving stock (Eq, Show, Read, Ord)

-- | This data will be sent back to you via webhook.
newtype GetStartedButton =
          GetStartedButton { getStartedPayload :: Text
                           -- ^ Payload sent back to your webhook in a
                           -- @messaging_postbacks@ event when the 'Get
                           -- Started' button is tapped. 1000 character limit.
                           }
  deriving stock (Eq, Show, Read, Ord)

-- | List of settings per locale
newtype PersistentMenu = PersistentMenu { menuItems :: [PersistentMenuSetting] }
  deriving stock (Eq, Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON, Semigroup)

-- | A setting that defines the persistent menu for a
-- certain locale. The menu with a locale property that
-- matches the person's locale will be displayed.
--
-- At least one object in the persistent_menu array must
-- specify @/"locale": "default"/@. This is the menu we
-- will fall back to if no object has a locale property
-- that matches the users locale.
--
-- __N.B. Just set `pmsLocale` to `Nothing` for the default__
data PersistentMenuSetting = PersistentMenuSetting
    { pmsLocale :: Maybe FBLocale
    -- ^ There can only be one PersistentMenuSetting
    -- with a certain locale. ('Nothing' == "default")
    --
    -- https://developers.facebook.com/docs/messenger-platform/messenger-profile/supported-locales
    , pmsInputDisabled :: Bool
    -- ^ Disables the Messenger composer field if set to 'True'.
    -- This means your app can only be interacted with via
    -- the persistent menu, postbacks, buttons, and webviews.
    -- (Default is `False`)
    , pmsCallToActions :: [PersistentMenuItem]
    -- ^ An array of top-level menu items for the persistent menu.
    -- A maximum of 3 items is allowed. A maximum of two nested menus are supported.
    --
    -- Required if 'pmsInputDisabled' is 'True'
    , pmsChatPluginDisabled :: Bool
    -- ^ If 'True', disables the persistent menu in the Customer Chat Plugin.
    } deriving stock (Eq, Show, Read, Ord)

-- | Constructor for the URL 'PersistentMenuItem'
persistentUrlItem :: Text
                  -- ^ /Menu item title. 30 character limit./
                  -> URL
                  -- ^ /This URL is opened in a mobile browser when the menu item is tapped./
                  -- /Must use HTTPS protocol if `MessengerExtensions is 'True'./
                  -> WebviewHeightRatioType
                  -- ^ /Height of the Webview. Valid values: 'COMPACT', 'TALL', 'FULL'. Default is 'FULL'/
                  -> Bool
                  -- ^ /Must be 'True' if using Messenger Extensions. Default is 'False'/
                  -> Maybe URL
                  -- ^ /The URL to use on clients that don't support Messenger Extensions./
                  -- /If this is not defined, the url will be used as the fallback./
                  -- /It may only be specified if 'ubMessengerExtensions' is 'True'./
                  -> WebviewShareType
                  -- ^ /Set to 'HIDE' to disable the share button in the Webview (for sensitive info)./
                  -- /This does not affect any shares initiated by the developer using Extensions./
                  -- /Default is 'SHOW'/
                  -> PersistentMenuItem
persistentUrlItem title url heightRatio msgExt fallbackUrl =
    PMIUrl . URLButton title url heightRatio msgExt fallbackUrl

-- | Shortcut function for a normal `web_url` menu item with the defaults
--
-- @persistentUrlItem_ title url = persistentUrlItem title url FULL False Nothing SHOW@
persistentUrlItem_ :: Text -> URL -> PersistentMenuItem
persistentUrlItem_ title url = persistentUrlItem title url FULL False Nothing SHOW

-- | URL menu item with Messenger Extensions activated.
--
-- @(URL,URL)@ are the URL using Messenger Extensions and the fallback URL
-- in case Messenger Extensions aren't supported on user's side.
persistentUrlItemME :: Text -- ^ /Menu item title. 30 character limit./
                    -> (URL,URL) -- ^ /(URL with Msgr Exts, Fallback URL w\/o Msgr Exts)/
                    -> WebviewHeightRatioType -- ^ /`FULL`\/`TALL`\/`COMPACT` (Default is `FULL`)/
                    -> WebviewShareType -- ^ /`HIDE`\/`SHOW` (Default is `SHOW`)/
                    -> PersistentMenuItem
persistentUrlItemME title (url,fallbackUrl) heightRatio =
    persistentUrlItem title url heightRatio True (Just fallbackUrl)

-- | Constructor for the Postback `TemplateButton`
--
-- Menu item used in Templates. Sends a callback to your server to act on it.
persistentPostbackItem :: Text -- ^ /Menu item title. 30 character limit./
                       -> Text -- ^ /This data will be sent back to your webhook. 1000 character limit./
                       -> PersistentMenuItem
persistentPostbackItem title = PMIPostback . PostbackButton title

-- | The different kinds of Persistent Menu items to choose from
data PersistentMenuItem = PMIUrl URLButton
                        | PMIPostback PostbackButton
                        | PMINested PersistentMenuItemNested
  deriving stock (Eq, Show, Read, Ord)

-- | Constructor for nested Persistent Menu items.
persistentNestedItem :: Text -- ^ /Menu item title. 30 char limit/
                     -> [PersistentMenuItem]
                     -- ^ /Nested `PersistentMenuItem` that will be expanded in next level./
                     -- /A maximum of 5 items is allowed. A persistent menu may have a maximum of two nested menus./
                     -> PersistentMenuItem
persistentNestedItem title = PMINested . PersistentMenuItemNested title

-- | A menu item that continues into more menu items.
data PersistentMenuItemNested = PersistentMenuItemNested
    { nestedMenuTitle :: Text -- ^ 30 char limit
    , nestedMenuCTA :: [PersistentMenuItem]
    -- ^ Nested `PersistentMenuItem` that will be expanded in next level.
    -- A maximum of 5 items is allowed. A persistent menu may have a maximum of two nested menus.
    } deriving stock (Eq, Show, Read, Ord)


-- | A list of domains being used. All domains must be valid. Up to 50 domains allowed.
newtype WhiteListedDomains = WhiteListedDomains { domains :: [URL] }
  deriving stock (Eq, Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON, Semigroup)


-- | URL opened by the Messenger Platform when a user triggers account linking.
newtype AccountLinkingUrl = AccountLinkingUrl { linkingUrl :: URL }
  deriving stock (Eq, Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)

-- | You need to be accepted to our beta program to use payment features in your bot in production.
-- You can still test payment features in development mode until then.
data PaymentSettings = PaymentSettings
    { psPrivacyUrl :: Maybe URL -- ^ The URL of the privacy policy for your app. Required for `BuyButton` payments.
    , psPublicKey :: Maybe Text
    -- ^ Your public key. Used to encrypt all webview payments,
    -- and `BuyButton` implementations that use tokenized payments.
    , psTesters :: [Text]
    -- ^ A list of IDs for people that will test payments in your app.
    -- These people will send a mock payment when they tap the `BuyButton`.
    } deriving stock (Eq, Show, Read, Ord)

-- | If 'taAudience' is `CUSTOM`, `tcBlacklist` and `tcWhitelist` can't both be null or empty.
-- In addition, only one of them can be non-empty at the same time.
data TargetAudience = TargetAudience
    { taAudience :: AudienceType -- ^ Valid values include `ALL`, `CUSTOM`, or `NONE`.
    , taCountries :: Maybe TargetCountries -- ^
    } deriving stock (Eq, Show, Read, Ord)

-- | The countries you'd want to target in the Discover tab
data TargetCountries = TargetCountries
    { tcWhitelist :: [Text] -- ^ List of ISO 3166 Alpha-2 codes.
    , tcBlacklist :: [Text] -- ^ List of ISO 3166 Alpha-2 codes.
    } deriving stock (Eq, Show, Read, Ord)

-- | The domain of the home URL for your Chat Extension must be added to the domain whitelist in your app's Messenger profile.
data HomeUrl = HomeUrl
    { hUrl :: URL
    -- ^ The URL to be invoked from drawer.
    --
    -- * Must be __whitelisted__.
    -- * Must use __https__.
    , hShareButton :: WebviewShareType -- ^ Controls whether the share button in the webview is enabled. Default is `HIDE`.
    , hInTest :: Bool
    -- ^ Controls whether users not assigned a role for your app or its Facebook page can see the Chat Extension.
    -- This should be set to true until the Chat Extension is ready to be used by others.
    } deriving stock (Eq, Show, Read, Ord)


-- ------------------------ --
--  SEND SETTING INSTANCES  --
-- ------------------------ --

instance ToJSON ProfileRequest where
  toJSON (ProfileRequest greet started menu doms linking payment audience home) =
      object' [ "greeting" .=!! greet
              , "get_started" .=!! started
              , "persistent_menu" .=!! menu
              , "whitelisted_domains" .=!! doms
              , "account_linking_url" .=!! linking
              , "payment_settings" .=!! payment
              , "target_audience" .=!! audience
              , "home_url" .=!! home
              ]

instance ToJSON GreetingSetting where
  toJSON (GreetingSetting mloc txt) =
      object [ "locale" .= maybe (String "default") toJSON mloc
             , "text" .= txt
             ]

instance ToJSON GetStartedButton where
  toJSON (GetStartedButton payload) = object [ "payload" .= payload ]


instance ToJSON PersistentMenuSetting where
  toJSON (PersistentMenuSetting mLocale input ctas chatDisabled) =
      object' $ [ "locale" .=! maybe (String "default") toJSON mLocale
                , mDefault "composer_input_disabled" False input
                , mEmptyList "call_to_actions" ctas
                ] ++
                ["disabled_surfaces" .=! [CUSTOMER_CHAT_PLUGIN] | chatDisabled]

instance ToJSON PersistentMenuItem where
  toJSON (PMIUrl x) = toJSON x
  toJSON (PMIPostback x) = toJSON x
  toJSON (PMINested x) = toJSON x

instance ToJSON PersistentMenuItemNested where
  toJSON (PersistentMenuItemNested title ctas) =
      object' [ "type" .=! String "nested"
              , "title" .=! title
              , mEmptyList "call_to_actions" ctas
              ]

instance ToJSON PaymentSettings where
  toJSON (PaymentSettings privacy key testers) =
      object' [ "privacy_url" .=!! privacy
              , "public_key" .=!! key
              , mEmptyList "testers" testers
              ]

instance ToJSON TargetAudience where
  toJSON (TargetAudience typ countries) =
      object' [ "audience_type" .=! typ
              , "countries" .=!! countries
              ]

instance ToJSON TargetCountries where
  toJSON (TargetCountries white black) =
      object' [ mEmptyList "whitelist" white
              , mEmptyList "blacklist" black
              ]

instance ToJSON HomeUrl where
  toJSON (HomeUrl url share test) =
      object' [ "url" .=! url
              , "webview_height_ratio" .=! TALL
              , mDefault "webview_share_button" HIDE share
              , "in_test" .=! test
              ]



instance FromJSON ProfileRequest where
  parseJSON = withObject "ProfileRequest" $ \o ->
      ProfileRequest <$> o .:? "greeting"
                     <*> o .:? "get_started"
                     <*> o .:? "persistent_menu"
                     <*> o .:? "whitelisted_domains"
                     <*> o .:? "account_linking_url"
                     <*> o .:? "payment_settings"
                     <*> o .:? "target_audience"
                     <*> o .:? "home_url"

instance FromJSON GreetingSetting where
  parseJSON = withObject "GreetingSetting" $ \o ->
      GreetingSetting <$> (localeP o >>= f)
                      <*> o .: "text"
    where localeP o = o .: "locale" :: Parser Text
          f x = if x == "default"
                  then pure Nothing
                  else parseJSON $ String x

instance FromJSON GetStartedButton where
  parseJSON = withObject "GetStartedButton" $ \o ->
      GetStartedButton <$> o .: "payload"

instance FromJSON PersistentMenuSetting where
  parseJSON = withObject "PersistentMenuSetting" $ \o ->
      PersistentMenuSetting <$> setLocale o
                            <*> o .:? "composer_input_disabled" .!= False
                            <*> o .:? "call_to_actions" .!= []
                            <*> disableChat o
    where setLocale o = localeP >>= f
            where localeP = o .: "locale" :: Parser Text
                  f x = if x == "default" then pure Nothing else parseJSON (String x)
          disableChat o = f <$> o .:? "disabled_surfaces"
            where f = \case
                        (Just [CUSTOMER_CHAT_PLUGIN]) -> True
                        _ -> False

instance FromJSON PersistentMenuItem where
  parseJSON = withObject "PersistentMenuItem" $ \o ->
        PMIUrl <$> parseJSON (Object o)
    <|> PMIPostback <$> parseJSON (Object o)
    <|> PMINested <$> parseJSON (Object o)

instance FromJSON PersistentMenuItemNested where
  parseJSON = withObject "PersistentMenuItemNested" $ \o ->
      PersistentMenuItemNested <$> o .: "title"
                               <*> o .:? "call_to_actions" .!= []

instance FromJSON PaymentSettings where
  parseJSON = withObject "PaymentSettings" $ \o ->
      PaymentSettings <$> o .:? "privacy_url"
                      <*> o .:? "public_key"
                      <*> o .:? "testers" .!= []

instance FromJSON TargetAudience where
  parseJSON = withObject "TargetAudience" $ \o ->
      TargetAudience <$> o .: "audience_type"
                     <*> o .:? "countries"

instance FromJSON TargetCountries where
  parseJSON = withObject "TargetCountries" $ \o ->
      TargetCountries <$> o .:? "whitelist" .!= []
                      <*> o .:? "blacklist" .!= []

instance FromJSON HomeUrl where
  parseJSON = withObject "HomeUrl" $ \o ->
      HomeUrl <$> o .: "url"
              <*> o .:? "webview_share_button" .!= HIDE
              <*> o .: "in_test"
