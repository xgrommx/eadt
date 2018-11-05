module EADT.Algebras.DisplayAlgebra where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Functor.Variant (VariantF)
import Data.Newtype (un)
import EADT.EADT (EADT(..))
import Heterogeneous.Folding as H
import Matryoshka (Algebra, cata)

class DisplayAlgebra (f :: Type -> Type) where
  displayAlgebra :: Algebra f (Cofree Array String)

data DisplayAlgebraCase = DisplayAlgebraCase

instance displayAlgebraCase :: DisplayAlgebra f => H.Folding DisplayAlgebraCase acc (f (Cofree Array String)) (Cofree Array String) where
  folding DisplayAlgebraCase _ = displayAlgebra

displayEADT :: forall r. H.HFoldl DisplayAlgebraCase Unit (VariantF r (Cofree Array String)) (Cofree Array String) => EADT r -> (Cofree Array String)
displayEADT t = cata alg (un EADT t)
  where
    alg :: H.HFoldl DisplayAlgebraCase Unit (VariantF r (Cofree Array String)) (Cofree Array String) => Algebra (VariantF r) (Cofree Array String)
    alg = H.hfoldl DisplayAlgebraCase unit