{-|
Module      : Web.Facebook.Messenger.Types.Requests.Attachment.Templates.OpenGraphTemplate
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

The Open Graph Template allows you to send a template message via an open graph URL plus an optional button.
Currently we only support share songs via open graph template and the song will appear in a bubble
that allows the users to see album art, preview the song, and click an optional button.

* __Implementation__:
This template message can be sent via @beginShareFlow()@ in Messenger Extensions JS SDK and via Send API.
The template relies on the developer providing a URL that contains Open Graph-formatted song details.
Messenger will read the following properties to populate the bubble:

    * @og:title@;
    * @og:audio@;
    * @music:musician@;
    * @og:site_name@;
    * and @og:image@

* __Availability__:
In the webview, to check whether the user's version of Messenger supports music messages,
call @getSupportedFeatures()@ and check for the key @"sharing_open_graph"@.

https://developers.facebook.com/docs/messenger-platform/open-graph-template
-}
module Web.Facebook.Messenger.Types.Requests.Attachment.Templates.OpenGraphTemplate (
  -- * Open Graph Template
  OpenGraphTemplate (..)
  , OpenGraphElement (..)
  )
where

import Control.Monad (unless)
import Data.Aeson
import Data.Text (Text)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests.Extra (TemplateButton)
import Web.Facebook.Messenger.Types.Static (URL)


-- ----------------------------- --
--  OPEN GRAPH TEMPLATE REQUEST  --
-- ----------------------------- --

-- | Template for sending a structured song bubble to a user
--
-- More info about the Open Graph: https://developers.facebook.com/docs/opengraph/music
newtype OpenGraphTemplate =
          OpenGraphTemplate { ogtElement :: OpenGraphElement }
  deriving (Eq, Show, Read, Ord)

instance ToJSON OpenGraphTemplate where
  toJSON (OpenGraphTemplate e) =
      object [ "template_type" .= String "open_graph"
             , "elements" .= [e]
             ]

instance FromJSON OpenGraphTemplate where
  parseJSON = checkValue
      "OpenGraphTemplate"
      "template_type"
      ("open_graph" :: Text)
      $ \o -> do
          elems <- o .: "elements"
          unless (length elems == 1) $
            fail "OpenGraphTemplate: amount of elements not 1"
          let [e] = elems
          -- This is safe because of the length check before it
          pure $ OpenGraphTemplate e

-- | URL to get the OpenGraph data from and optional buttons
data OpenGraphElement = OpenGraphElement
    { ogeUrl :: URL -- ^ Open graph URL for the element
    , ogeButtons :: [TemplateButton] -- ^ Maximum of 3 buttons are allowed when sending via Send API.
    } deriving (Eq, Show, Read, Ord)

instance ToJSON OpenGraphElement where
  toJSON (OpenGraphElement url buttons) =
      object' [ "url" .=! url
              , mEmptyList "buttons" $ take 3 buttons
              ]

instance FromJSON OpenGraphElement where
  parseJSON = withObject "OpenGraphElement" $ \o ->
      OpenGraphElement <$> o .: "url"
                       <*> o .:? "buttons" .!= []

