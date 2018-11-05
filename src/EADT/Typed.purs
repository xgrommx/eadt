module EADT.Typed where

import Prelude

import Control.Comonad.Cofree as Cofree
import Data.Functor.Variant (VariantF)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant.Internal (FProxy)
import EADT.Algebras.DisplayAlgebra (class DisplayAlgebra)
import EADT.Algebras.ShowAlgebra (class ShowAlgebra, ShowAlgebraCase, showEADT)
import EADT.Algebras.TypeCheckGAlgebra (class TypeCheckGAlgebra)
import EADT.EADT (EADT, injEADT)
import EADT.Type (Type(..))
import Heterogeneous.Folding as H

data TypedF e = TypedF Type e

derive instance functorTypeF :: Functor TypedF

type Typed r = (typed :: FProxy TypedF | r)

_typed = SProxy :: SProxy "typed"

typed :: forall r. Type -> EADT (Typed r) -> EADT (Typed r)
typed t e = injEADT _typed (TypedF t e)

instance showAlgebraTypedF :: ShowAlgebra TypedF where
  showAlgebra (TypedF t e) = "(" <> e <> " :: " <> show t <> ")"

instance typeCheckGAlgebraTypedF :: H.HFoldl ShowAlgebraCase Unit (VariantF r String) String => TypeCheckGAlgebra TypedF r where
  typeCheckGAlgebra (TypedF t (Tuple e te))
    | t == te   = t
    | TError _ <- t  = t
    | TError _ <- te = te
    | otherwise = TError $ "wrong type ascription " <> show t <> " for expression `" <> showEADT e <> "` with inferred type " <> show te

instance displayAlgebraTypedF :: DisplayAlgebra TypedF where
  displayAlgebra (TypedF t c) = Cofree.mkCofree ((Cofree.head c) <> " :: " <> show t) (Cofree.tail c)