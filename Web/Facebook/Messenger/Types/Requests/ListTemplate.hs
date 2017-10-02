{-|
Module      : Web.Facebook.Messenger.Types.Requests.ListTemplate
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

The `ListTemplate` is a template that allows you to present a set of items vertically.

It can be rendered in two different ways:

(1) @--[`ListLARGE`]@ The first way renders the first item with a cover image with text overlaid.
This is good to making the first item appear prominently above the other items.
(2) @[`ListCOMPACT`]@ The second way renders each item identically and is useful for presenting a list of items where no item is shown prominently.

Each item may render a button which can be used as a call-to-action. You can also provide a URL to be opened when an item is tapped.
Each `ListTemplate` message can also have a global `TemplateButton` which will be rendered below the item list.

Please take note of the following:

* Each element also supports a `DefaultAction`. Using this, you can enable people to open a URL when the row of the list item is tapped.
* You may send at least 2 and at most 4 `ListElement`s.
* Adding a `TemplateButton` to each `ListElement` is optional. You may only have up to 1 `TemplateButton` per `ListElement`.
* You may have up to 1 global `TemplateButton`.
-}
module Web.Facebook.Messenger.Types.Requests.ListTemplate (
  -- * List Template
  ListTemplate (..)
  , ListElement (..)
  )
where

import Data.Aeson
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Requests.Extra (TemplateButton, DefaultAction)
import Web.Facebook.Messenger.Types.Static


-- ----------------------- --
--  LIST TEMPLATE REQUEST  --
-- ----------------------- --

-- | Template for sending a list with `ListElement`s; maybe containing images and/or links.
data ListTemplate = ListTemplate
    { ltTopElementStyle :: ListStyle -- ^ Value must be `ListLARGE` or `ListCOMPACT`. Default to `ListLARGE` if not specified.
    , ltElements :: [ListElement] -- ^ List view elements (minimum of 2 elements and maximum of 4 elements)
    , ltButton :: Maybe TemplateButton  -- ^ Button associated on the `ListTemplate` message.
    } deriving (Eq, Show)

instance ToJSON ListTemplate where
  toJSON (ListTemplate style elements button) =
      object' [ "template_type" .=! String "list"
              , "top_element_style" .=! style
              , "elements" .=! go elements
              , "buttons" .=!! fmap (:[]) button
              ]
    where go [e] = [e,e]
          go es  = take 4 es
  
instance FromJSON ListTemplate where
  parseJSON = checkValue
      "ListTemplate"
      "template_type"
      ("list" :: Text)
      $ \o -> do
          buttons <- o .:? "buttons" .!= []
          let button = listToMaybe buttons
          ListTemplate <$> o .:? "top_element_style" .!= ListLARGE
                              <*> o .: "elements"
                              <*> pure button

-- | Elements used in the `ListTemplate`
data ListElement = ListElement
    { leTitle :: Text -- ^ Title of the element (80 character limit)
    , leSubtitle :: Maybe Text -- ^ Subtitle of the element (80 character limit)
    , leImageUrl :: Maybe URL -- ^ URL to image in list view item. It is required for the first element if `ListStyle` is large
    , leDefaultAction :: Maybe DefaultAction -- ^ Default action to be triggered when user taps on the element row.
    , leButton :: Maybe TemplateButton -- ^ Button on the element
    } deriving (Eq, Show)

instance ToJSON ListElement where
  toJSON (ListElement title subtitle image da button) =
      object' [ "title" .=! title
              , "subtitle" .=!! subtitle
              , "image_url" .=!! image
              , "default_action" .=!! da
              , "buttons" .=!! fmap (:[]) button
              ]

instance FromJSON ListElement where
  parseJSON = withObject "ListElement" $ \o -> do
      buttons <- o .:? "buttons" .!= []
      let button = listToMaybe buttons
      ListElement <$> o .: "title"
                  <*> o .:? "subtitle"
                  <*> o .:? "image_url"
                  <*> o .:? "default_action"
                  <*> pure button
