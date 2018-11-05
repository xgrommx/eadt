module EADT.Algebras.TypeCheckAlgebra2 where

import Prelude

import Data.Functor.Variant (VariantF)
import Data.Newtype (un)
import EADT.EADT (EADT(..))
import EADT.Typed (TypedF)
import Heterogeneous.Folding as H
import Matryoshka (Algebra, cata)

class TypeCheckAlgebra2 (f :: Type -> Type) (ys :: # Type) where
  typeCheckAlgebra2 :: Algebra f (TypedF (EADT ys))

data TypeCheckAlgebra2Case = TypeCheckAlgebra2Case

instance typeCheckAlgebra2Case :: TypeCheckAlgebra2 f r => H.Folding TypeCheckAlgebra2Case acc (f (TypedF (EADT r))) (TypedF (EADT r)) where
  folding TypeCheckAlgebra2Case _ = typeCheckAlgebra2

typeCheck3 :: forall r. H.HFoldl TypeCheckAlgebra2Case Unit (VariantF r (TypedF (EADT r))) (TypedF (EADT r)) => EADT r -> (TypedF (EADT r))
typeCheck3 t = cata alg (un EADT t) where
  alg :: H.HFoldl TypeCheckAlgebra2Case Unit (VariantF r (TypedF (EADT r))) (TypedF (EADT r)) => Algebra (VariantF r) (TypedF (EADT r))
  alg = H.hfoldl TypeCheckAlgebra2Case unit