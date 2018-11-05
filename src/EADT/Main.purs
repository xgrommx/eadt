module EADT.Main where

import Data.Array
import Prelude hiding (add, mul)

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Env (EnvT(..))
import Data.Maybe (maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import EADT.Add (Add, add)
import EADT.Algebras.DisplayAlgebra (displayEADT)
import EADT.Algebras.EvalAlgebra (evalEADT)
import EADT.Algebras.ShowAlgebra (showEADT)
import EADT.Algebras.TypeCheckAlgebra (typeCheck)
import EADT.Algebras.TypeCheckAlgebra2 (typeCheck3)
import EADT.Algebras.TypeCheckGAlgebra (typeCheck2)
import EADT.EADT (EADT, injEADT)
import EADT.Mul (Mul, mul)
import EADT.NVal (NVal, nval)
import EADT.Type (Type(..))
import EADT.Typed (Typed, _typed, typed)
import EADT.Val (Val, val)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Matryoshka (cata)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type E = EADT (Add + Val + ())
type E1 = EADT (Add + Val + NVal + ())
type E2 = EADT (Add + Val + NVal + Mul + ())
type E3 = EADT (Add + Val + NVal + Mul + Typed + ())

unlines :: Array String -> String
unlines = joinWith "\n"

drawTree :: forall a. Show a => Cofree Array a -> Array String
drawTree = cata alg where
  alg (EnvT (Tuple x xs)) = show x : foldMap (\t -> pure "|" <> shift "+- " "|  " t) xs
  shift h t tr = uncons tr # maybe mempty (\r -> (h <> r.head) : map (t <> _) (r.tail))

expr :: E
expr = ((add (val 10) (val 30)) :: E)

main :: Effect Unit
main = do
  log $ showEADT expr
  logShow $ evalEADT expr
  log $ showEADT ((add (val 10) (nval 30.0)) :: E1)
  logShow $ typeCheck ((add (val 10) (val 20)) :: E1)
  logShow $ typeCheck ((add (val 10) (nval 20.0)) :: E1)
  logShow $ typeCheck2 $ (add (nval 10.0) (nval 20.0)) :: E1
  logShow $ typeCheck2 $ (add (nval 10.0) (val 30)) :: E1
  logShow $ typeCheck2 $ (mul (nval 10.0) (nval 30.0)) :: E2
  logShow $ typeCheck2 $ (add (nval 10.0) (typed TInt $ val 5)) :: E3
  logShow $ typeCheck2 $ (add (nval 10.0) (typed TNumber $ nval 5.0)) :: E3
  logShow $ showEADT ((injEADT _typed $ unsafeCoerce $ typeCheck3 (((add (val 10) (val 20)) :: E1))) :: E3)
  logShow $ showEADT ((injEADT _typed $ unsafeCoerce $ typeCheck3 (((add (val 10) (nval 20.0)) :: E1))) :: E3)
  logShow $ showEADT ((injEADT _typed $ unsafeCoerce $ typeCheck3 ((add (add (mul (nval 2.0) (nval 3.0)) (val 10)) (val 5)) :: E2)) :: E3)
  log $ unlines $ drawTree $ displayEADT ((injEADT _typed $ unsafeCoerce $ typeCheck3 ((add (add (mul (nval 2.0) (val 3)) (val 10)) (val 5)) :: E2)) :: E3)