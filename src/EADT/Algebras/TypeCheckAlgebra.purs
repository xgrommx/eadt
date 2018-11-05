module EADT.Algebras.TypeCheckAlgebra where

import Prelude

import Data.Functor.Variant (VariantF)
import Data.Newtype (un)
import EADT.EADT (EADT(..))
import EADT.Type (Type)
import Heterogeneous.Folding as H
import Matryoshka (Algebra, cata)

class TypeCheckAlgebra (f :: Type -> Type) where
  typeCheckAlgebra :: Algebra f Type

data TypeCheckAlgebraCase = TypeCheckAlgebraCase

instance typeCheckAlgebraCase :: TypeCheckAlgebra f => H.Folding TypeCheckAlgebraCase acc (f Type) Type where
  folding TypeCheckAlgebraCase _ = typeCheckAlgebra

typeCheck :: forall r. H.HFoldl TypeCheckAlgebraCase Unit (VariantF r Type) Type => EADT r -> Type
typeCheck t = cata alg (un EADT t) where
  alg :: H.HFoldl TypeCheckAlgebraCase Unit (VariantF r Type) Type => Algebra (VariantF r) Type
  alg = H.hfoldl TypeCheckAlgebraCase unit