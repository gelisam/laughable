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
--     testLeft
--       :: (Show c, Show b, Show j, Eq c)
--       => (c -> a -> (b, j))
--       -> DissectedList c a j
--       -> IO (DissectedList c b j)
--     testLeft f d = do
--       case ( d & extendLeft (\maybeC _ -> (maybeC, undefined))
--                & view focus
--            , d & left (\c _ -> (c, undefined))
--                & preview (_Just . focus)
--            , d & left f
--            )
--            of
--         (Nothing, _, _) -> do
--           error "testLeft: extendLeft thinks we're at the leftmost position"
--         (_, Nothing, _) -> do
--           error "testLeft: left thinks we're at the leftmost position"
--         (_, _, Nothing) -> do
--           error "testLeft: left thinks we're at the leftmost position"
--         (Just expectedC, Just actualC, Just d')
--           | expectedC /= actualC -> do
--             error "testLeft: extendLeft and left gave different clowns"
--           | otherwise -> do
--             printDissectedList d'
--             pure d'
--     testRight
--       :: (Show c, Show b, Show j, Eq j)
--       => (a -> j -> (c, b))
--       -> DissectedList c a j
--       -> IO (DissectedList c b j)
--     testRight f d = do
--       case ( d & extendRight (\_ maybeJ -> (undefined, maybeJ))
--                & view focus
--            , d & right (\_ j -> (undefined, j))
--                & preview (_Just . focus)
--            , d & right f
--            )
--            of
--         (Nothing, _, _) -> do
--           error "testRight: extendRight thinks we're at the rightmost position"
--         (_, Nothing, _) -> do
--           error "testRight: right thinks we're at the rightmost position"
--         (_, _, Nothing) -> do
--           error "testRight: right thinks we're at the rightmost position"
--         (Just expectedJ, Just actualJ, Just d')
--           | expectedJ /= actualJ -> do
--             error "testRight: extendRight and right gave different jokers"
--           | otherwise -> do
--             printDissectedList d'
--             pure d'
--     testExtendLeft
--       :: (Show c, Show b, Show j)
--       => (a -> (b, j))
--       -> DissectedList c a j
--       -> IO (DissectedList c b j)
--     testExtendLeft f d = do
--       case ( d & left undefined
--            , d & extendLeft (\maybeC _ -> (maybeC, undefined))
--                & view focus
--            , d & extendLeft (\Nothing a -> f a)
--            )
--            of
--         (Just _, _, _) -> do
--           error "testExtendLeft: left thinks we're not at the leftmost position"
--         (_, Just _, _) -> do
--           error "testExtendLeft: left thinks we're not at the leftmost position"
--         (Nothing, Nothing, d') -> do
--           printDissectedList d'
--           pure d'
--     testExtendRight
--       :: (Show c, Show b, Show j)
--       => (a -> (c, b))
--       -> DissectedList c a j
--       -> IO (DissectedList c b j)
--     testExtendRight f d = do
--       case ( d & right undefined
--            , d & extendRight (\_ maybeJ -> (undefined, maybeJ))
--                & view focus
--            , d & extendRight (\a Nothing -> f a)
--            )
--            of
--         (Just _, _, _) -> do
--           error "testExtendRight: right thinks we're not at the rightmost position"
--         (_, Just _, _) -> do
--           error "testExtendRight: right thinks we're not at the rightmost position"
--         (Nothing, Nothing, d') -> do
--           printDissectedList d'
--           pure d'
-- :}
--
-- >>> import Control.Monad
-- >>> :{
-- singleton 'a' & ( testExtendRight (\a -> (2, succ a))
--               >=> testExtendRight (\a -> (3, succ a))
--               >=> testLeft (\c a -> (succ a, c * 10))
--               >=> testLeft (\c a -> (succ a, c * 10))
--               >=> testExtendLeft (\a -> (succ a, 10))
--               >=> testRight (\a c -> (c `div` 10, succ a))
--               >=> testRight (\a c -> (c `div` 10, succ a))
--               >=> testRight (\a c -> (c `div` 10, succ a))
--                 )
-- :}
-- [2] 'b' []
-- [2,3] 'c' []
-- [2] 'd' [30]
-- [] 'e' [20,30]
-- [] 'f' [10,20,30]
-- [1] 'g' [20,30]
-- [1,2] 'h' [30]
-- [1,2,3] 'i' []


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

left
  :: (c -> a -> (b, j))
  -> DissectedList c a j
  -> Maybe (DissectedList c b j)
left f d = do
  clownSeq' :> c <- pure (d ^. clownSeq)
  let a          = d ^. focus
  let (b, j)     = f c a
  let jokerList' = j : (d ^. jokerList)
  pure $ DissectedList clownSeq' b jokerList'

right
  :: (a -> j -> (c, b))
  -> DissectedList c a j
  -> Maybe (DissectedList c b j)
right f d = do
  let a           = d ^. focus
  j :< jokerList' <- pure (d ^. jokerList)
  let (c, b)      = f a j
  let clownSeq'   = (d ^. clownSeq) |> c
  pure $ DissectedList clownSeq' b jokerList'

extendLeft
  :: (Maybe c -> a -> (b, j))
  -> DissectedList c a j
  -> DissectedList c b j
extendLeft f d
  = let (clownSeq', maybeC) = case d ^. clownSeq of
          clownSeq' :> c
            -> (clownSeq', Just c)
          _ -> (mempty, Nothing)
        (focus', j) = f maybeC (d ^. focus)
    in DissectedList clownSeq' focus' (j : d ^. jokerList)

extendRight
  :: (a -> Maybe j -> (c, b))
  -> DissectedList c a j
  -> DissectedList c b j
extendRight f d
  = let (maybeJ, jokerList') = case d ^. jokerList of
          j : jokerList'
            -> (Just j, jokerList')
          _ -> (Nothing, mempty)
        (c, focus') = f (d ^. focus) maybeJ
    in DissectedList (d ^. clownSeq |> c) focus' jokerList'


clowns
  :: Traversal (DissectedList c a j) (DissectedList c' a j) c c'
clowns
  = clownSeq . each

jokers
  :: Traversal (DissectedList c a j) (DissectedList c a j') j j'
jokers
  = jokerList . each
