module EADT.Val where

import Prelude

import Control.Comonad.Cofree (mkCofree)
import Data.Int (toNumber)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import EADT.Algebras.DisplayAlgebra (class DisplayAlgebra)
import EADT.Algebras.EvalAlgebra (class EvalAlgebra)
import EADT.Algebras.ShowAlgebra (class ShowAlgebra)
import EADT.Algebras.TypeCheckAlgebra (class TypeCheckAlgebra)
import EADT.Algebras.TypeCheckAlgebra2 (class TypeCheckAlgebra2)
import EADT.Algebras.TypeCheckGAlgebra (class TypeCheckGAlgebra)
import EADT.EADT (EADT, injEADT)
import EADT.Type (Type(..))
import EADT.Typed (TypedF(..))
import Matryoshka (Algebra)
import Prim.Row as Row
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

data ValF e = ValF Int 
derive instance functorValF :: Functor ValF

type Val r = (val :: FProxy ValF | r)

_val = SProxy :: SProxy "val"

val :: forall r. Int -> EADT (Val r)
val n = injEADT _val (ValF n)

instance showAlgebraValF :: ShowAlgebra ValF where
  showAlgebra (ValF i) = show i

instance evalAlgebraValF :: EvalAlgebra ValF where
  evalAlgebra (ValF i) = toNumber i

instance typeCheckAlgebraValF :: TypeCheckAlgebra ValF where
  typeCheckAlgebra _ = TInt

instance typeCheckGAlgebraValF :: TypeCheckGAlgebra ValF r where
  typeCheckGAlgebra _ = TInt

instance typeCheckAlgebra2ValF :: Row.Cons "val" a r1 r2 => TypeCheckAlgebra2 ValF r1 where
  typeCheckAlgebra2 :: Algebra ValF (TypedF (EADT r1))
  typeCheckAlgebra2 (ValF i) = TypedF TInt ((unsafeCoerce $ val i) :: EADT r1)

instance displayAlgebraValF :: DisplayAlgebra ValF where
  displayAlgebra (ValF i) = mkCofree (show i) []