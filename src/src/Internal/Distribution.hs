
module  Internal.Distribution
    ( IDist ( MKIDist)
    , uniformFromList
    , certain
    , empty
    , bind
    , union
    ) where

import qualified Data.Map  as M
import qualified Data.List as L
import Data.Function

newtype IDist prob val = MKIDist { getIDist :: M.Map val prob }
  deriving (Read, Show, Eq)

certain :: Num prob => val -> IDist prob val
certain x = MKIDist $ M.singleton x 1

empty :: IDist prob val
empty = MKIDist M.empty

mapOnValues :: (Num prob, Ord val') => (val -> val') -> IDist prob val -> IDist prob val'
mapOnValues f = MKIDist . M.mapKeysWith (+) f . getIDist

mapOnProbabilities :: (prob -> prob) -> IDist prob val -> IDist prob val
mapOnProbabilities f = MKIDist . M.map f . getIDist

getProbOf :: Ord k => k -> IDist prob k -> Maybe prob
getProbOf x = M.lookup x . getIDist

uniformFromList :: (Fractional prob, Ord val) => [val] -> IDist prob val
uniformFromList  xs = MKIDist . M.fromList $ L.map (\x -> (x, 1 / L.genericLength xs)) xs


scale :: (Num val', Num prob, Ord val') => val' -> IDist prob val' -> IDist prob val'
scale factor = mapOnValues (*factor)

union :: (Num prob, Ord val) => IDist prob val -> IDist prob val -> IDist prob val
union dx dy = MKIDist $ M.unionWith (+) (getIDist dx) (getIDist dy)


bind :: (Eq prob, Num prob, Ord val) => IDist prob t -> (t -> IDist prob val) -> IDist prob val
bind dx fd = MKIDist $ M.fromListWith (+)  unnormalizedList
  where unnormalizedList = [ (y , pydx * px ) | (x,px) <-M.toList $ getIDist dx, (y, pydx) <- M.toList $ getIDist $ fd x ]
