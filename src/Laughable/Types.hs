{-# LANGUAGE TemplateHaskell #-}
module Laughable.Types
  ( module Laughable.Types
  , Dissected, Bisected
  ) where

import Control.Lens
import Data.Void

import Laughable.Internal.Types


-- | Guarantees that there is at least one element.
newtype NonEmpty t a = NonEmpty
  { unNonEmpty :: Dissected t Void a a }

-- | Guarantees that there is exactly one element.
newtype Singleton t a = Singleton
  { unSingleton :: Dissected t Void a Void }

-- |
-- Guarantees that there is at least one element, the one at the cursor. You
-- can move the cursor left and right in the order prescribed by 'traverse' and
-- apply @a -> a@ modifications to the focussed element.
newtype Zipper t a = Zipper
  { unZipper :: Dissected t a a a }

makePrisms ''NonEmpty
makePrisms ''Singleton
makePrisms ''Zipper
