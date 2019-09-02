{-# LANGUAGE LambdaCase, TemplateHaskell #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Laughable.Internal.SplitTraversable where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

import Laughable.Internal.DissectedList (DissectedList(DissectedList))
import qualified Laughable.Internal.DissectedList as DissectedList


data SplitTraversable t c a j = SplitTraversable
  { _shape  :: t ()
  , _values :: DissectedList c a j
  }

makeLenses ''SplitTraversable


fromTraversable
  :: Traversable t
  => t j
  -> SplitTraversable t c () j
fromTraversable tj
  = SplitTraversable
  { _shape  = tj & traversed .~ ()
  , _values = DissectedList mempty () (tj ^.. traversed)
  }

-- |
-- >>> replaceValues [(),(),()] ['a', 'b']
-- Nothing
-- >>> replaceValues [(),(),()] ['a', 'b', 'c']
-- Just "abc"
-- >>> replaceValues [(),(),()] ['a', 'b', 'c', 'd']
-- Just "abc"
replaceValues
  :: Traversable t
  => t () -> [a] -> Maybe (t a)
replaceValues tu as
  = runIdentity
  $ runMaybeT
  $ flip evalStateT as
  $ flip traverse tu
  $ \() -> do
      get >>= \case
        [] -> do
          empty
        (a:as) -> do
          put as
          pure a


clowns
  :: Traversal (SplitTraversable t c a j) (SplitTraversable t c' a j) c c'
clowns
  = values . DissectedList.clowns

jokers
  :: Traversal (SplitTraversable t c a j) (SplitTraversable t c a j') j j'
jokers
  = values . DissectedList.jokers
