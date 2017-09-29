{-|
Module      : Web.Facebook.Messenger.Types
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This module provides a way to construct a compounded request for the Messenger Profile API.

The Messenger Profile for your app is where you set properties that define various aspects of the following Messenger Platform features.
-}
module Web.Facebook.Messenger.Types (
  -- * Exported modules
  module Web.Facebook.Messenger.Types.Responses
  , module Web.Facebook.Messenger.Types.Callbacks
  , module Web.Facebook.Messenger.Types.Requests
  , module Web.Facebook.Messenger.Types.Static
  ) where

import Data.Text

import Web.Facebook.Messenger.Types.Callbacks
import Web.Facebook.Messenger.Types.Requests
import Web.Facebook.Messenger.Types.Responses
import Web.Facebook.Messenger.Types.Static
