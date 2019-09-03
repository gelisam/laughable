{-# OPTIONS -Wno-name-shadowing #-}
module Laughable.Bisected where

import Control.Lens

import Laughable.Internal.Types
import Laughable.Internal.DissectedList (DissectedList(..))
import Laughable.Internal.SplitTraversable (SplitTraversable(..))
import qualified Laughable.Internal.DissectedList as DissectedList
import qualified Laughable.Internal.SplitTraversable as SplitTraversable


fromTraversable
  :: Traversable t
  => t j
  -> Bisected t c j
fromTraversable
  = Bisected
  . SplitTraversable.fromTraversable


halfLeft
  :: Bisected t c j
  -> Either (Bisected t void j)
            (Dissected t c c j)
halfLeft (Bisected (SplitTraversable tu vs))
  = case DissectedList.climbLeft vs of
      Left vs
        -> Left $ Bisected $ SplitTraversable tu vs
      Right vs
        -> Right $ Dissected $ SplitTraversable tu vs

halfRight
  :: Bisected t c j
  -> Either (Bisected t c void)
            (Dissected t c j j)
halfRight (Bisected (SplitTraversable tu vs))
  = case DissectedList.climbRight vs of
      Left vs
        -> Left $ Bisected $ SplitTraversable tu vs
      Right vs
        -> Right $ Dissected $ SplitTraversable tu vs

left
  :: (c -> j)
  -> Bisected t c j
  -> Either (Bisected t void j)
            (Bisected t c j)
left f b = do
  Dissected s <- halfLeft b
  pure $ Bisected
       $ s & SplitTraversable.values . DissectedList.focus %~ f
           & SplitTraversable.values %~ DissectedList.slideLeft

right
  :: (j -> c)
  -> Bisected t c j
  -> Either (Bisected t c void)
            (Bisected t c j)
right f b = do
  Dissected s <- halfRight b
  pure $ Bisected
       $ s & SplitTraversable.values . DissectedList.focus %~ f
           & SplitTraversable.values %~ DissectedList.slideRight


clowns
  :: Traversal (Bisected t c j) (Bisected t c' j) c c'
clowns
  = _Bisected . SplitTraversable.values . DissectedList.clowns

jokers
  :: Traversal (Bisected t c j) (Bisected t c j') j j'
jokers
  = _Bisected . SplitTraversable.values . DissectedList.jokers

elements
  :: Traversal (Bisected t a a) (Bisected t b b) a b
elements f (Bisected (SplitTraversable tu (DissectedList cs () js)))
    = Bisected
  <$> SplitTraversable tu
  <$> (DissectedList <$> traverse f cs <*> pure () <*> traverse f js)
