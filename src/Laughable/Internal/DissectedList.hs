{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Laughable.Internal.DissectedList where

import Control.Lens
import Data.Sequence (Seq)

-- $setup
-- >>> :{
-- let printDissectedList
--       :: (Show c, Show a, Show j)
--       => DissectedList c a j
--       -> IO ()
--     printDissectedList d = do
--       putStr $ show (d ^.. clowns)
--       putStr " "
--       putStr $ show (d ^. focus)
--       putStr " "
--       putStr $ show (d ^.. jokers)
--       putStrLn ""
--     testTransform
--       :: (Show c, Show b, Show j)
--       => (a -> DissectedList c b j)
--       -> a
--       -> IO (DissectedList c b j)
--     testTransform f a = do
--       d <- pure (f a)
--       printDissectedList d
--       pure d
--     testClimbLeft
--       :: (Show c, Show j)
--       => DissectedList c () j
--       -> IO (DissectedList c c j)
--     testClimbLeft d = do
--       case climbLeft d of
--         Nothing -> do
--           error "testClimbLeft: climbLeft thinks we're at the leftmost position"
--         Just d -> do
--           printDissectedList d
--           pure d
--     testClimbRight
--       :: (Show c, Show j)
--       => DissectedList c () j
--       -> IO (DissectedList c j j)
--     testClimbRight d = do
--       case climbRight d of
--         Nothing -> do
--           error "testClimbRight: climbRight thinks we're at the rightmost position"
--         Just d -> do
--           printDissectedList d
--           pure d
--     testLeftmost
--       :: (Show c, Show j)
--       => DissectedList c () j
--       -> IO (DissectedList c () j)
--     testLeftmost d = do
--       case climbLeft d of
--         Nothing -> do
--           pure d
--         Just _ -> do
--           error "testClimbLeft: climbLeft does not think we're at the leftmost position"
--     testRightmost
--       :: (Show c, Show j)
--       => DissectedList c () j
--       -> IO (DissectedList c () j)
--     testRightmost d = do
--       case climbRight d of
--         Nothing -> do
--           pure d
--         Just _ -> do
--           error "testClimbRight: climbRight does not think we're at the rightmost position"
-- :}
--
-- >>> import Control.Monad
-- >>> :{
-- singleton () & ( testRightmost
--              >=> testTransform (focus .~ 2)
--              >=> testTransform slideRight
--              >=> testRightmost
--              >=> testTransform (focus .~ 3)
--              >=> testTransform slideRight
--              >=> testRightmost
--              >=> testClimbLeft
--              >=> testTransform (focus %~ (* 10))
--              >=> testTransform slideLeft
--              >=> testClimbLeft
--              >=> testTransform (focus %~ (* 10))
--              >=> testTransform slideLeft
--              >=> testLeftmost
--              >=> testTransform (focus .~ 1)
--              >=> testTransform slideRight
--              >=> testClimbRight
--              >=> testTransform (focus %~ (`div` 10))
--              >=> testTransform slideRight
--              >=> testClimbRight
--              >=> testTransform (focus %~ (`div` 10))
--              >=> testTransform slideRight
--              >=> testRightmost
--                )
-- :}
-- [] 2 []
-- [2] () []
-- [2] 3 []
-- [2,3] () []
-- [2] 3 []
-- [2] 30 []
-- [2] () [30]
-- [] 2 [30]
-- [] 20 [30]
-- [] () [20,30]
-- [] 1 [20,30]
-- [1] () [20,30]
-- [1] 20 [30]
-- [1] 2 [30]
-- [1,2] () [30]
-- [1,2] 30 []
-- [1,2] 3 []
-- [1,2,3] () []


-- | Zero or more 'c's, one 'a', then zero or more 'j's.
data DissectedList c a j = DissectedList
  { _clownSeq  :: Seq c
  , _focus     :: a
  , _jokerList :: [j]
  }

makeLenses ''DissectedList


singleton
  :: a -> DissectedList c a j
singleton a
  = DissectedList mempty a mempty


-- Imagine there is a sequence of elements with empty space around and
-- in-between each of them:
--
--    _o_o_o_o_
--
-- If we are in an empty space, we can climb to the top of an element, but that
-- may fail if there is no element in that direction. If we are already on top
-- of an element, we may slide down the the empty space on either side, that
-- operation will always succeed.

climbLeft
  :: DissectedList c () j
  -> Maybe (DissectedList c c j)
climbLeft d = do
  clownSeq' :> c <- pure (d ^. clownSeq)
  pure $ DissectedList clownSeq' c (d ^. jokerList)

climbRight
  :: DissectedList c () j
  -> Maybe (DissectedList c j j)
climbRight d = do
  j :< jokerList' <- pure (d ^. jokerList)
  pure $ DissectedList (d ^. clownSeq) j jokerList'

slideLeft
  :: DissectedList c j j
  -> DissectedList c () j
slideLeft d
  = DissectedList (d ^. clownSeq)
                  ()
                  (d ^. focus <| d ^. jokerList)

slideRight
  :: DissectedList c c j
  -> DissectedList c () j
slideRight d
  = DissectedList (d ^. clownSeq |> d ^. focus)
                  ()
                  (d ^. jokerList)


clowns
  :: Traversal (DissectedList c a j) (DissectedList c' a j) c c'
clowns
  = clownSeq . each

jokers
  :: Traversal (DissectedList c a j) (DissectedList c a j') j j'
jokers
  = jokerList . each
