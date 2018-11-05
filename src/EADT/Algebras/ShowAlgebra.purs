module EADT.Algebras.ShowAlgebra where

import Prelude

import Data.Functor.Variant (VariantF)
import Data.Newtype (un)
import EADT.EADT (EADT(..))
import Heterogeneous.Folding as H
import Matryoshka (Algebra, cata)

class ShowAlgebra (f :: Type -> Type) where
  showAlgebra :: Algebra f String

data ShowAlgebraCase = ShowAlgebraCase

instance showAlgebraCase :: ShowAlgebra f => H.Folding ShowAlgebraCase acc (f String) String where
  folding ShowAlgebraCase _ = showAlgebra

showEADT :: forall r. H.HFoldl ShowAlgebraCase Unit (VariantF r String) String => EADT r -> String
showEADT t = cata alg (un EADT t)
  where
    alg :: H.HFoldl ShowAlgebraCase Unit (VariantF r String) String => Algebra (VariantF r) String
    alg = H.hfoldl ShowAlgebraCase unit