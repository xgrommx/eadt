module EADT.Type where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

data Type
  = TInt
  | TNumber
  | TError String

derive instance genericType :: Generic Type _

instance eqType :: Eq Type where
  eq x y = genericEq x y

instance showType :: Show Type where
  show x = genericShow x