
{-# LANGUAGE GADTs #-}

module  Distribution
  where

import Internal.Distribution          as R
import qualified Data.List            as L
import qualified Data.Map             as M
import qualified Data.Functor         as F
import qualified Control.Applicative  as A
import qualified Data.Foldable        as Foldable

import Data.Monoid
import Data.Foldable (Foldable)
import Control.Monad

data Dist prob a where
  Zero   :: Dist prob a
  Return :: a -> Dist prob a
  Prim   :: R.IDist prob a -> Dist prob a
  Plus   :: Dist prob a -> Dist prob a -> Dist prob a
  Bind   :: Dist prob b -> (b -> Dist prob a) -> Dist prob a

--afihew9hf9whef


run :: (Eq prob, Ord val, Num prob) => Dist prob val -> IDist prob val
run Zero                  = R.empty
run (Return a)            = R.certain a
run (Prim d)              = d
run (Plus Zero a)         = run a
run (Plus a Zero)         = run a
run (Plus a b)            = R.union (run a) (run b)
run (Bind Zero _)         = run Zero
run (Bind (Return a) f)   = run $ f a
run (Bind (Prim da)  f)   = R.bind da (run . f)
run (Bind (Plus da db) f) = run (Plus (Bind da f) (Bind db f))
run (Bind (Bind da f) g)  = run (Bind da (\a -> Bind (f a) g))

--run (Bind dx fd)        = R.bind (run dx) (run . fd)

instance F.Functor (Dist prob) where
  fmap = liftM

instance A.Applicative (Dist prob) where
  pure  = return
  (<*>) = ap

instance A.Alternative (Dist prob) where
  empty = Zero
  (<|>) = Plus

instance Monad (Dist prob) where
  return = Return
  (>>=)  = Bind

certain :: a -> Dist prob a
certain = return

fromList :: Ord a => [(a,prob)] -> Dist prob a
fromList = Prim . MKIDist . M.fromList
