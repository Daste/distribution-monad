{-# LANGUAGE GADTs #-}
{-|
Module      : Distribution
Description : A monadic probability distribution type (Works like a set with a probability annotation)
Copyright   : (c) Dario Sterzi 2017
License     : BSD3
Maintainer  : sterzi.dario@gmail.com
Stability   : experimental
-}

module  Distribution  ( Dist
                      , certain
                      , fromList
                      , fromMap
                      , toList
                      , toMap
                      ) where

import qualified Internal.Distribution as R
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Functor          as F
import qualified Control.Applicative   as A
import qualified Data.Foldable         as Foldable

import Data.Map ( (!) )

import Data.Monoid
import Data.Foldable (Foldable)
import Control.Monad

data Dist prob a where
  Zero   ::                                      Dist prob a
  Return :: a                                 -> Dist prob a
  Prim   :: R.IDist prob a                    -> Dist prob a
  Plus   :: Dist prob a -> Dist prob a        -> Dist prob a
  Bind   :: Dist prob b -> (b -> Dist prob a) -> Dist prob a

run :: (Eq prob, Ord val, Num prob) => Dist prob val -> R.IDist prob val
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

instance F.Functor (Dist prob) where
  fmap = liftM

instance A.Applicative (Dist prob) where
  pure  = return
  (<*>) fm xm = do
    f <- fm
    x <- xm
    return $ f x

instance A.Alternative (Dist prob) where
  empty = Zero
  (<|>) = Plus

instance Monad (Dist prob) where
  return = Return
  (>>=)  = Bind

certain :: a -> Dist prob a
certain = return

fromList :: Ord a => [(a,prob)] -> Dist prob a
fromList = fromMap . M.fromList

fromMap :: M.Map a prob -> Dist prob a
fromMap = Prim . R.MKIDist

toList :: (Eq prob, Ord a, Num prob) =>  Dist prob a -> [(a, prob)]
toList = M.toList . toMap

toMap :: (Eq prob, Ord a, Num prob) =>  Dist prob a -> M.Map a prob
toMap = R.getIDist . run

applyWithList :: (Eq prob1, Num prob1, Ord a1, Ord a) => ([(a1, prob1)] -> [(a, prob)])
                                                       -> Dist prob1 a1 -> Dist prob a
applyWithList f = fromList . f . toList

normalize :: (Eq prob, Fractional  prob, Ord a) => Dist prob a -> Dist prob a
normalize = applyWithList normalizer
    where
      normalizer list = map (\ (val, prob) -> (val, prob/tot) ) list
        where tot = sum $ map snd list

-- | Get an unsafe function from the set of values to theyr probabilities
probabilityAsFunction :: (Eq prob, Num prob, Ord a) => Dist prob a -> a -> prob
probabilityAsFunction dist = ( toMap dist !)

-- | Get a safe function from the set of values to their probabilities if they are defined
probabilityAsFunctionM :: (Eq prob, Num prob, Ord a) => Dist prob a -> a -> Maybe prob
probabilityAsFunctionM dist = flip M.lookup ( toMap dist )

-- | Get the elements in a list
toElementsList :: (Eq prob, Num prob, Ord a) => Dist prob a -> [a]
toElementsList  d = map fst $ toList d

-- | Get the probabilities in a list
toProbsList :: (Eq b, Num b, Ord a) => Dist b a -> [b]
toProbsList d = map snd $ toList d
