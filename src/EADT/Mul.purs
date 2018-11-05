module EADT.Mul where

import Prelude

import Control.Comonad.Cofree (mkCofree)
import Data.Functor.Variant (VariantF)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant.Internal (FProxy)
import EADT.Algebras.DisplayAlgebra (class DisplayAlgebra)
import EADT.Algebras.EvalAlgebra (class EvalAlgebra)
import EADT.Algebras.ShowAlgebra (class ShowAlgebra, ShowAlgebraCase, showEADT)
import EADT.Algebras.TypeCheckAlgebra2 (class TypeCheckAlgebra2)
import EADT.Algebras.TypeCheckGAlgebra (class TypeCheckGAlgebra)
import EADT.EADT (EADT, injEADT)
import EADT.Type (Type(..))
import EADT.Typed (TypedF(..), typed)
import Heterogeneous.Folding as H
import Prim.Row as Row
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

data MulF e = MulF e e
derive instance functorMulF :: Functor MulF

type Mul r = (mul :: FProxy MulF | r)

_mul = SProxy :: SProxy "mul"

mul :: forall r. EADT (Mul + r) -> EADT (Mul + r) -> EADT (Mul + r)
mul x y = injEADT _mul (MulF x y)

instance showAlgebraMulF :: ShowAlgebra MulF where
  showAlgebra (MulF u v) = "(" <> u <> " * " <> v <> ")"

instance evalAlgebraMulF :: EvalAlgebra MulF where
  evalAlgebra (MulF u v) = u * v

instance typeCheckGAlgebraValMulF :: H.HFoldl ShowAlgebraCase Unit (VariantF r String) String => TypeCheckGAlgebra MulF r where
  typeCheckGAlgebra (MulF (Tuple u t1) (Tuple v t2))
    | t1 == t2 = t1
    | TError _ <- t1 = t1
    | TError _ <- t2 = t2
    | otherwise = TError $ "can't mul `" <> showEADT u <> "` whose type is " <> show t1 <> " with `" <> showEADT v <> "` whose type is " <> show t2

instance typeCheckAlgebra2MulF :: (Row.Cons "mul" a r ys, H.HFoldl ShowAlgebraCase Unit (VariantF r String) String) => TypeCheckAlgebra2 MulF r where
  typeCheckAlgebra2 (MulF u@(TypedF t1 e1) v@(TypedF t2 e2))
    | t1 == t2 = TypedF t1 $ unsafeCoerce (mul (typed t1 (unsafeCoerce e1)) (typed t2 (unsafeCoerce e2)))
    | TError _ <- t1 = TypedF (TError "") $ unsafeCoerce (mul (typed t1 (unsafeCoerce e1)) (typed t2 (unsafeCoerce e2)))
    | TError _ <- t2 = TypedF (TError "") $ unsafeCoerce (mul (typed t1 (unsafeCoerce e1)) (typed t2 (unsafeCoerce e2)))
    | otherwise = TypedF (TError $ "can't mul " <> show t1 <> " with " <> show t2) $ unsafeCoerce (mul (typed t1 (unsafeCoerce e1)) (typed t2 (unsafeCoerce e2)))

instance displayAlgebraMulF :: DisplayAlgebra MulF where
  displayAlgebra (MulF u v) = mkCofree "(*)" [u,v]