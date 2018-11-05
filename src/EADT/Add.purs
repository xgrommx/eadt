module EADT.Add where

import Prelude

import Control.Comonad.Cofree (mkCofree)
import Data.Functor.Variant (VariantF)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant.Internal (FProxy)
import EADT.Algebras.DisplayAlgebra (class DisplayAlgebra)
import EADT.Algebras.EvalAlgebra (class EvalAlgebra)
import EADT.Algebras.ShowAlgebra (class ShowAlgebra, ShowAlgebraCase, showEADT)
import EADT.Algebras.TypeCheckAlgebra (class TypeCheckAlgebra)
import EADT.Algebras.TypeCheckAlgebra2 (class TypeCheckAlgebra2)
import EADT.Algebras.TypeCheckGAlgebra (class TypeCheckGAlgebra)
import EADT.EADT (EADT, injEADT)
import EADT.Type (Type(..))
import EADT.Typed (TypedF(..), typed)
import Heterogeneous.Folding as H
import Prim.Row as Row
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

data AddF e = AddF e e
derive instance functorAddF :: Functor AddF

type Add r = (add :: FProxy AddF | r)

_add = SProxy :: SProxy "add"

add :: forall r. EADT (Add + r) -> EADT (Add + r) -> EADT (Add + r)
add x y = injEADT _add (AddF x y)

instance showAlgebraAddF :: ShowAlgebra AddF where
  showAlgebra (AddF u v) = "(" <> u <> " + " <> v <> ")"

instance evalAlgebraAddF :: EvalAlgebra AddF where
  evalAlgebra (AddF u v) = u + v

instance typeCheckAlgebrakAddF :: TypeCheckAlgebra AddF where
  typeCheckAlgebra (AddF t1 t2)
    | t1 == t2       = t1
    | TError _ <- t1 = t1
    | TError _ <- t2 = t2
    | otherwise      = TError $ "can't match " <> show t1 <> " with " <> show t2

instance typeCheckGAlgebraValAddF :: H.HFoldl ShowAlgebraCase Unit (VariantF r String) String => TypeCheckGAlgebra AddF r where
  typeCheckGAlgebra (AddF (Tuple u t1) (Tuple v t2))
    | t1 == t2 = t1
    | TError _ <- t1 = t1
    | TError _ <- t2 = t2
    | otherwise = TError $ "can't add `" <> showEADT u <> "` whose type is " <> show t1 <> " with `" <> showEADT v <> "` whose type is " <> show t2 

instance typeCheckAlgebra2AddF :: (Row.Cons "add" a r ys, H.HFoldl ShowAlgebraCase Unit (VariantF r String) String) => TypeCheckAlgebra2 AddF r where
  typeCheckAlgebra2 (AddF u@(TypedF t1 e1) v@(TypedF t2 e2))
    | t1 == t2 = TypedF t1 $ unsafeCoerce (add (typed t1 (unsafeCoerce e1)) (typed t2 (unsafeCoerce e2)))
    | TError _ <- t1 = TypedF (TError "") $ unsafeCoerce (add (typed t1 (unsafeCoerce e1)) (typed t2 (unsafeCoerce e2)))
    | TError _ <- t2 = TypedF (TError "") $ unsafeCoerce (add (typed t1 (unsafeCoerce e1)) (typed t2 (unsafeCoerce e2)))
    | otherwise = TypedF (TError $ "can't add " <> show t1 <> " with " <> show t2) $ unsafeCoerce (add (typed t1 (unsafeCoerce e1)) (typed t2 (unsafeCoerce e2)))

instance displayAlgebraAddF :: DisplayAlgebra AddF where
  displayAlgebra (AddF u v) = mkCofree "(+)" [u,v]