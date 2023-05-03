{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.AppRoles
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

When using Account Linking, this callback will occur when the Link Account or Unlink Account button have been tapped.
The status parameter tells you whether the user linked or unlinked their account.
The authorization_code is a pass-through parameter, allowing you to match the business user entity to the page-scoped ID (PSID) of the sender.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_account_linking
-}
module Web.Facebook.Messenger.Types.Callbacks.AccountLink (
  -- * Account Linking Callback
  AccountLink (..)
  )
where


import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text

import Web.Facebook.Messenger.Internal


-- -------------------------- --
--  ACCOUNT LINKING CALLBACK  --
-- -------------------------- --

-- | The linking or unlinking of a user with an account.
-- `AccountLink` might have a pass-through authorization_code provided in the Linking Account flow
data AccountLink = AccountLink { authCode :: Maybe Text }
                 | AccountUnlink
  deriving stock (Eq, Show, Read, Ord)

-- --------------------------- --
--  ACCOUNT LINKING INSTANCES  --
-- --------------------------- --

instance FromJSON AccountLink where
  parseJSON = withObject "AccountLink" $ \o -> do
    status <- o .: "status" :: Parser Text
    case status of
      "unlinked" -> pure AccountUnlink
      "linked" -> AccountLink <$> o .:? "authorization_code"
      wat -> fail $ "AccountLink: unexpected \"status\" string: " `mappend` unpack wat


instance ToJSON AccountLink where
  toJSON accLink = object'
      [ "status" .=! String txt
      , "authorization_code" .=!! code
      ]
    where (txt, code) = go accLink
          go = \case
                AccountLink mCode -> ("linked", mCode)
                AccountUnlink -> ("unlinked",Nothing)
