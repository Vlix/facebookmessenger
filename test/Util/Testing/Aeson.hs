{-# LANGUAGE TemplateHaskell #-}

module Util.Testing.Aeson where

import Data.Aeson.Types
import Data.Text hiding (map)
import Language.Haskell.TH
import qualified Test.QuickCheck.Property as Prop
import Test.Tasty.QuickCheck hiding (Success)

-- | Create a QuickCheck property that tests if the FromJSON instance
-- can parse what the ToJSON emits, and that the parsed value
-- is equal (through Eq) to the original.
jsonTest :: (Eq a, Show a, Arbitrary a, FromJSON a, ToJSON a) => Text -> proxy a -> Prop.Property
jsonTest lbl proxy = label (unpack lbl) $ \a ->
    let _check = typecheck proxy a
    in case fromJSON (toJSON a) of
         Error str -> Prop.failed { Prop.reason = "JSON parsing: " ++ str }
         Success a' | a == a'   -> Prop.succeeded
                    | otherwise -> Prop.failed { Prop.reason = "parseJSON returned different value than original\nReparsed:\n" ++ show a' ++ "\nOriginal:\n" ++ show a ++ "\n\n"}
  where typecheck :: f b -> b -> ()
        typecheck _ _ = ()

-- | TH helper that generates a list of QuickCheck-properties from a list of type names.
--
mkJSONTests :: [Name] -> Q Exp
mkJSONTests tys = listE $ map mkJSONTest tys

-- | TH helper that generates a single property from a type name.
mkJSONTest :: Name -> Q Exp
mkJSONTest ty = [|jsonTest (pack $(litE $ stringL $ nameBase ty)) (Nothing :: Maybe $(conT ty))|]

-- | TH helper that generates a list of @TestTree@s from a list of type names.
-- Usage:
--
-- @
-- main :: IO ()
-- main = Tasty.defaultMain tests
--
-- tests :: Tasty.TestTree
-- tests = Tasty.testGroup "Aeson" $(mkJSONTestTrees [''Data, ''Type, ''Names, ''Here])
-- @
mkJSONTestTrees :: [Name] -> Q Exp
mkJSONTestTrees tys = listE $ map mkJSONTestTree tys

-- | TH helper that generates a single @TestTree@ from a type name.
mkJSONTestTree :: Name -> Q Exp
mkJSONTestTree ty = [|testProperty $(litE $ stringL $ nameBase ty) $(mkJSONTest ty)|]

