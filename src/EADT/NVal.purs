module EADT.NVal where

import Prelude

import Control.Comonad.Cofree (mkCofree)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import EADT.Algebras.DisplayAlgebra (class DisplayAlgebra)
import EADT.Algebras.ShowAlgebra (class ShowAlgebra)
import EADT.Algebras.TypeCheckAlgebra (class TypeCheckAlgebra)
import EADT.Algebras.TypeCheckAlgebra2 (class TypeCheckAlgebra2)
import EADT.Algebras.TypeCheckGAlgebra (class TypeCheckGAlgebra)
import EADT.EADT (EADT, injEADT)
import EADT.Type (Type(..))
import EADT.Typed (TypedF(..))
import Matryoshka (Algebra)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

data NValF e = NValF Number 
derive instance functorNValF :: Functor NValF

type NVal r = (nval :: FProxy NValF | r)

_nval = SProxy :: SProxy "nval"

nval :: forall r. Number -> EADT (NVal r)
nval n = injEADT _nval (NValF n)

instance showAlgebraNValF :: ShowAlgebra NValF where
  showAlgebra (NValF i) = show i

instance typeCheckAlgebraValF :: TypeCheckAlgebra NValF where
  typeCheckAlgebra _ = TNumber

instance typeCheckGAlgebraValIntF :: TypeCheckGAlgebra NValF r where
  typeCheckGAlgebra _ = TNumber

instance typeCheckAlgebra2ValF :: Row.Cons "val" a r1 r2 => TypeCheckAlgebra2 NValF r1 where
  typeCheckAlgebra2 :: Algebra NValF (TypedF (EADT r1))
  typeCheckAlgebra2 (NValF i) = TypedF TNumber ((unsafeCoerce $ nval i) :: EADT r1)

instance displayAlgebraValF :: DisplayAlgebra NValF where
  displayAlgebra (NValF i) = mkCofree (show i) []