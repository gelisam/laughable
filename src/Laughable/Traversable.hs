module Laughable.Traversable where

import Control.Lens

import Laughable.Internal.SplitTraversable
import Laughable.Internal.Types
import qualified Laughable.Bisected as Bisected
import qualified Laughable.Dissected as Dissected


fromDissected
  :: Traversable t
  => Dissected t a a a
  -> t a
fromDissected b
  = case replaceValues (b ^. _Dissected . shape) (b ^.. Dissected.elements) of
      Just ta
        -> ta
      Nothing
        -> error "Traversable.fromDissected: never happens, Dissected's invariant guarantees that it holds enough values"

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
