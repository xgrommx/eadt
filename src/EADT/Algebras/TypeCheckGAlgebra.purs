module EADT.Algebras.TypeCheckGAlgebra where

import Prelude

import Data.Bifunctor (lmap)
import Data.Functor.Variant (VariantF)
import Data.Newtype (un, wrap)
import Data.Tuple (Tuple)
import EADT.EADT (EADT(..))
import EADT.Type (Type)
import Heterogeneous.Folding as H
import Matryoshka (GAlgebra, para)

class TypeCheckGAlgebra (f :: Type -> Type) (r :: # Type) where
  typeCheckGAlgebra :: GAlgebra (Tuple (EADT r)) f Type

data TypeCheckGAlgebraCase = TypeCheckGAlgebraCase

instance typeCheckGAlgebraCase :: TypeCheckGAlgebra f r => H.Folding TypeCheckGAlgebraCase acc (f (Tuple (EADT r) Type)) Type where
  folding TypeCheckGAlgebraCase _ = typeCheckGAlgebra

typeCheck2 :: forall r. H.HFoldl TypeCheckGAlgebraCase Unit (VariantF r (Tuple (EADT r) Type)) Type => EADT r -> Type
typeCheck2 t = para (alg <<< map (lmap wrap)) (un EADT t)
  where
    alg :: H.HFoldl TypeCheckGAlgebraCase Unit (VariantF r (Tuple (EADT r) Type)) Type => GAlgebra (Tuple (EADT r)) (VariantF r) Type
    alg = H.hfoldl TypeCheckGAlgebraCase unit