module EADT.EADT where

import Prelude

import Data.Functor.Mu (Mu, roll)
import Data.Functor.Variant (FProxy, SProxy, VariantF, inj)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol)
import Matryoshka (Algebra)
import Prim.Row as Row

newtype EADT t = EADT (Mu (VariantF t))

derive instance newtypeEADT :: Newtype (EADT t) _

injEADT :: forall f s a b. Row.Cons s (FProxy f) a b => IsSymbol s => Functor f => SProxy s -> Algebra f (EADT b)
injEADT label alg = wrap (roll $ inj label (unwrap <$> alg))

-- liftEADT :: forall t f' u f. Recursive t (VariantF f) => Row.Union f u f' => t -> Mu (VariantF f')
-- liftEADT = cata (In <<< expand)