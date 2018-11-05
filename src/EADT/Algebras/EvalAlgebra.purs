module EADT.Algebras.EvalAlgebra where

import Prelude

import Data.Functor.Variant (VariantF)
import Data.Newtype (un)
import EADT.EADT (EADT(..))
import Heterogeneous.Folding as H
import Matryoshka (Algebra, cata)

class EvalAlgebra (f :: Type -> Type) where
  evalAlgebra :: Algebra f Number

data EvalAlgebraCase = EvalAlgebraCase

instance evalAlgebraCase :: EvalAlgebra f => H.Folding EvalAlgebraCase acc (f Number) Number where
  folding EvalAlgebraCase _ = evalAlgebra

evalEADT :: forall r. H.HFoldl EvalAlgebraCase Unit (VariantF r Number) Number => EADT r -> Number
evalEADT t = cata alg (un EADT t)
  where
    alg :: H.HFoldl EvalAlgebraCase Unit (VariantF r Number) Number => Algebra (VariantF r) Number
    alg = H.hfoldl EvalAlgebraCase unit