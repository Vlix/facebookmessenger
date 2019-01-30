{-|
Module      : Web.Facebook.Messenger.Internal
Copyright   : (c) Felix Paulusma, 2018
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This module contains some functions that help in keeping
'FromJSON' instances concise.

-}
module Web.Facebook.Messenger.Internal (
  -- * Helper functions
  --
  -- | These functions are mostly convenience functions to make @JSON@ as short as possible
  -- and to make writing certain `FromJSON` instances less painful.

  -- ** ToJSON functions
  object'
  , (.=!)
  , (.=!!)
  , mDefault
  , mEmptyList
  -- ** FromJSON functions
  , checkValue
  , withText'
  , withTextCI
  )
where


import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text, toUpper, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.HashMap.Strict as HM

-- | Helper function to avoid @`Maybe` [a]@ when an empty list doesn't have to (or shouldn't) be included in the @JSON@
mEmptyList :: ToJSON a => Text -> [a] -> Maybe Pair
mEmptyList _ [] = Nothing
mEmptyList t l  = Just $ t .= l

-- | Helper function to not include values that are the default when not included in the @JSON@
mDefault :: (Eq a, ToJSON a) => Text -> a -> a -> Maybe Pair
mDefault t a b = if a == b then Nothing else Just $ t .= b

-- | Alternative to "Aeson"'s `object` to make a @JSON object@ that might omit certain fields.
-- `Just` `Pair` will be included in the @JSON object@. `Nothing` will not.
--
-- @
-- `object'` [ "type" `.=!` `Data.Aeson.String` "image"
--         , "url" `.=!` url
--         , "title" `.=!!` mTitle
--         , `mEmptyList` "elements" elements
--         , `mDefault` "notification_type" `REGULAR` notifType
--         ]
-- @
--
-- The above will result in the following in case @mTitle@ is `Nothing`, @elements@ is @[]@ and @notifType@ is `REGULAR`:
--
-- @
-- {
--   "type": "image",
--   "url": "http:\/\/www.example.com\/image.jpg"
-- }
-- @
--
-- Compaired to when using the regular "Aeson"'s `object`:
--
-- @
-- {
--   "type": "image",
--   "url": "http:\/\/www.example.com\/image.jpg",
--   "title": null,
--   "elements": [],
--   "notification_type": "regular"
-- }
-- @
object' :: [Maybe Pair] -> Value
object' = Object . HM.fromList . catMaybes

-- | @a `.=!!` b@ will omit the specified `Pair` in case @b@ is `Nothing`
(.=!!) :: ToJSON a => Text -> Maybe a -> Maybe Pair
(.=!!) name = fmap (name .=)

-- | Add a required `Pair` to the @JSON object@
(.=!) :: ToJSON a => Text -> a -> Maybe Pair
(.=!) name = Just . (name .=)

-- | This function checks to see if a `Value` is an `Object` and then proceeds to check
-- if a certain field has a certain value before continuing parsing the object.
-- (e.g. checking if @"type"@ is actually @"image"@ or not)
checkValue :: (FromJSON a, ToJSON a, Eq a)
           => String -- ^ /reference in case the parsing fails/
           -> Text -- ^ /field name to check/
           -> a -- ^ /value to check in that field/
           -> (Object -> Parser b) -- ^ /parser to run in case the field check succeeds/
           -> Value
           -> Parser b
checkValue fName field value f = withObject fName $ \o -> do
    typ <- o .: field
    unless (typ == value) $
     fail $ fName <> ": wrong " <> show field <> " value: " <> showJson typ
    f o
  where showJson = unpack . decodeUtf8 . toStrict . encode

-- | Shortcut function for parsing certain sum types.
--
-- @
-- instance `FromJSON` `SenderActionType` where
--   `parseJSON` = `withText'` \"SenderActionType\"
--       [("mark_seen", `MARK_SEEN`)
--       ,("typing_on", `TYPING_ON`)
--       ,("typing_off", `TYPING_OFF`)
--       ]
-- @
withText' :: String -- ^ /reference in case the parsing fails/
          -> [(Text, a)] -- ^ /lookup list of JSON String to sum type/
          -> Value
          -> Parser a
withText' s tups = withText s $ \t ->
    case lookup t tups of
      Just val -> pure val
      _ -> fail $ "Wrong String for " <> s <> ": " <> unpack t

-- | Like 'withText'', but case insensitive.
withTextCI :: String -- ^ /reference in case the parsing fails/
           -> [(Text, a)] -- ^ /lookup list of JSON String to sum type/
           -> Value
           -> Parser a
withTextCI s tups = withText s $ \t ->
    case lookItUp t of
      Just val -> pure val
      _ -> fail $ "Wrong String for " <> s <> ": " <> unpack t
  where lookItUp t = toUpper t `lookup` (first toUpper <$> tups)
