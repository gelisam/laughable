module Laughable.Traversable where

import Control.Lens

import Laughable.Internal.SplitTraversable
import Laughable.Internal.Types
import qualified Laughable.Bisected as Bisected


fromBisected
  :: Traversable t
  => Bisected t a a
  -> t a
fromBisected b
  = case replaceValues (b ^. _Bisected . shape) (b ^.. Bisected.elements) of
      Just ta
        -> ta
      Nothing
        -> error "Traversable.fromBisected: never happens, Bisected's invariant guarantees that it holds enough values"
