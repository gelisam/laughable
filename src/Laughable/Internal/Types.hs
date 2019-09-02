{-# LANGUAGE TemplateHaskell #-}
module Laughable.Internal.Types where

import Control.Lens

import Laughable.Internal.SplitTraversable (SplitTraversable)


-- |
-- A zipper which supports @a -> b@ modifications. All the elements to the left
-- of the cursor have type @c@ and all the elements to the right of the focus
-- have type @j@, so you must change the cursor to a @j@ in order to move left,
-- or to a @c@ in order to move right. Useful for converting a @t j@ into a @t
-- c@ one element at a time.
newtype Dissected t c a j = Dissected
  -- invariant: lengthOf shape
  --         == lengthOf (values . clowns)
  --          + 1
  --          + lengthOf (values . jokers)
  { unDissected :: SplitTraversable t c a j
  }

-- |
-- A variant of 'Dissected' in which the cursor is in-between two elements
-- rather than on an element. Use a @c -> j@ to move left, or a @j -> c@ to
-- move right. This allows the container to be empty.
newtype Bisected t c j = Bisected
  -- invariant: lengthOf shape
  --         == lengthOf (values . clowns)
  --          + lengthOf (values . jokers)
  { unBisected :: SplitTraversable t c () j
  }

makePrisms ''Dissected
makePrisms ''Bisected
