module Web.Facebook.Messenger.Types.Callbacks.CheckoutUpdate
  ( CheckoutUpdate (..) )
where


import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.Text

import Web.Facebook.Messenger.Types.Requests.Extra (TemplateAddress)


-- | This callback is used for flexible-amount transactions
data CheckoutUpdate = CheckoutUpdate
    { cuPayload :: Text
    , cuShipid  :: Text
  -- Again, the documentation example is a Number, and the text says the shipping_address_id's a String...
    , cuAddress :: TemplateAddress
    } deriving (Eq, Show)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON CheckoutUpdate where
  toJSON (CheckoutUpdate payload shipid address) =
      object [ "payload" .= payload
             , "shipping_address" .= addIDtoAddress
             ]
    where addIDtoAddress = changeObject addressValue $ HM.insert "id" shipIDValue
          shipIDValue    = toJSON shipid
          addressValue   = toJSON address
          changeObject (Object o) f = Object $ f o
          changeObject x          _ = x

instance FromJSON CheckoutUpdate where
  parseJSON = withObject "CheckoutUpdate" $ \o -> do
      shipAddr <- o .: "shipping_address"
      case shipAddr of
        Object ob -> do
            ident <- ob .: "id"
            case ident of
              i@(Number _) ->
                  CheckoutUpdate <$> o .: "payload"
                                 <*> fmap (pack . show) (parseJSON i :: Parser Int)
                                 <*> o .: "shipping_address"
              (String t) ->
                  CheckoutUpdate <$> o .: "payload"
                                 <*> pure t
                                 <*> o .: "shipping_address"
              _ -> fail "CheckoutUpdate: invalid type in \"id\" field."
        _ -> fail "CheckoutUpdate: \"shipping_address\" value not an object."
