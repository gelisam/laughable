{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Laughable.Internal.DissectedList where

import Control.Lens
import Data.Sequence (Seq)

-- $setup
-- >>> :{
-- let test :: Show a
--          => DissectedList Int a Int -> IO ()
--     test d = do
--       putStr $ show (d ^.. clowns)
--       putStr " "
--       putStr $ show (d ^. focus)
--       putStr " "
--       putStr $ show (d ^.. jokers)
--       putStrLn ""
-- :}
--
-- >>> let d0       = singleton 'a'
-- >>> let d1       = d0    & right (\a Nothing -> (1, succ a))
-- >>> let d2       = d1    & right (\a Nothing -> (2, succ a))
-- >>> let d3       = d2    & right (\a Nothing -> (3, succ a))
-- >>> let Just d2' = d3    & left (\c a -> (succ a, c * 10))
-- >>> let Just d1' = d2'   & left (\c a -> (succ a, c * 10))
-- >>> let Just d0' = d1'   & left (\c a -> (succ a, c * 10))
-- >>> let d1''     = d0'   & right (\a (Just c) -> (c `div` 10, succ a))
-- >>> let d2''     = d1''  & right (\a (Just c) -> (c `div` 10, succ a))
-- >>> let d3''     = d2''  & right (\a (Just c) -> (c `div` 10, succ a))
-- >>> mapM_ test [d0, d1, d2, d3, d2', d1', d0', d1'', d2'', d3'']
-- [] 'a' []
-- [1] 'b' []
-- [1,2] 'c' []
-- [1,2,3] 'd' []
-- [1,2] 'e' [30]
-- [1] 'f' [20,30]
-- [] 'g' [10,20,30]
-- [1] 'h' [20,30]
-- [1,2] 'i' [30]
-- [1,2,3] 'j' []


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
  :: (a -> Maybe j -> (c, b))
  -> DissectedList c a j
  -> DissectedList c b j
right f d
  = let (maybeJ, jokerList') = case d ^. jokerList of
          j : jokerList'
            -> (Just j, jokerList')
          _ -> (Nothing, mempty)
        (c, focus') = f (d ^. focus) maybeJ
    in DissectedList (d ^. clownSeq |> c) focus' jokerList'


clowns
  :: Traversal' (DissectedList c a j) c
clowns
  = clownSeq . each

jokers
  :: Traversal' (DissectedList c a j) j
jokers
  = jokerList . each
