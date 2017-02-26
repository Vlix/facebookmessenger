module Web.Facebook.Messenger.Types.Callbacks.CheckoutUpdate where


import Data.Aeson
import Data.Aeson.Types     (typeMismatch, Parser)
import Data.Text
import Data.HashMap.Strict  as HM

import Web.Facebook.Messenger.Types.Requests.Templates (TemplateAddress)


-- | This callback is used for flexible-amount transactions
data CheckoutUpdate = CheckoutUpdate
  { checkup_payload :: Text
  , checkup_shipid  :: Text
-- Again, the documentation example is a Number, and the text says the shipping_address_id's a String...
  , checkup_address :: TemplateAddress
  } deriving (Eq, Show)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON CheckoutUpdate where
  toJSON (CheckoutUpdate payload shipid address) =
    object [ "payload"          .= payload
           , "shipping_address" .= addIDtoAddress
           ]
    where addIDtoAddress = flip changeObject addressValue $ insert "id" shipIDValue
          shipIDValue    = toJSON shipid
          addressValue   = toJSON address
          changeObject f (Object o) = Object $ f o
          changeObject _ x          = x

instance FromJSON CheckoutUpdate where
  parseJSON = withObject "CheckoutUpdate" $ \o ->
    case HM.lookup "shipping_address" o of
      Just (Object ob) ->
        case HM.lookup "id" ob of
          Just i@(Number _) ->
            CheckoutUpdate <$> o .: "payload"
                           <*> (pack . show <$> (parseJSON i :: Parser Int))
                           <*> o .: "shipping_address"
          Just (String t) ->
            CheckoutUpdate <$> o .: "payload"
                           <*> pure t
                           <*> o .: "shipping_address"
          _ -> fail "Invalid type in 'id' field of CheckoutUpdate object."
      _ -> fail "No shipping_address key or no id key in shipping_address object of CheckoutUpdate object."
