{-# OPTIONS -Wno-name-shadowing #-}
module Laughable.Bisected where

import Control.Lens

import Laughable.Internal.Types
import Laughable.Internal.DissectedList (DissectedList(..))
import Laughable.Internal.SplitTraversable (SplitTraversable(..))
import qualified Laughable.Internal.DissectedList as DissectedList
import qualified Laughable.Internal.SplitTraversable as SplitTraversable

-- $setup
-- >>> :{
-- let printBisected
--       :: (Show c, Show j)
--       => Bisected t c j
--       -> IO ()
--     printBisected d = do
--       putStr $ show (d ^.. clowns)
--       putStr " "
--       putStr $ show (d ^.. jokers)
--       putStrLn ""
--     testLeft
--       :: (Show c, Show j)
--       => (c -> j)
--       -> Bisected t c j
--       -> IO (Bisected t c j)
--     testLeft f b = do
--       case left f b of
--         Left _ -> do
--           error "testLeft: left thinks we're at the leftmost position"
--         Right b -> do
--           printBisected b
--           pure b
--     testRight
--       :: (Show c, Show j)
--       => (j -> c)
--       -> Bisected t c j
--       -> IO (Bisected t c j)
--     testRight f b = do
--       case right f b of
--         Left _ -> do
--           error "testRight: right thinks we're at the rightmost position"
--         Right b -> do
--           printBisected b
--           pure b
--     testLeftmost
--       :: Bisected t c j
--       -> IO (Bisected t void j)
--     testLeftmost d = do
--       case left undefined d of
--         Left d -> do
--           pure d
--         Right _ -> do
--           error "testLeftmost: left does not think we're at the leftmost position"
--     testRightmost
--       :: Bisected t c j
--       -> IO (Bisected t c void)
--     testRightmost d = do
--       case right undefined d of
--         Left d -> do
--           pure d
--         Right _ -> do
--           error "testRightmost: right does not think we're at the rightmost position"
-- :}
--
-- >>> import Control.Monad
-- >>> import qualified Laughable.Traversable as Traversable
-- >>> :{
-- fromTraversable [1,2,3] & ( testLeftmost
--                         >=> testRight (* 10)
--                         >=> testRight (* 10)
--                         >=> testRight (* 10)
--                         >=> testRightmost
--                         >=> testLeft (`div` 10)
--                         >=> testLeft (`div` 10)
--                         >=> testLeft (`div` 10)
--                         >=> testLeftmost
--                         >=> testRight show
--                         >=> testRight show
--                         >=> testRight show
--                         >=> testRightmost
--                         >=> pure . Traversable.fromBisected
--                           )
-- :}
-- [10] [2,3]
-- [10,20] [3]
-- [10,20,30] []
-- [10,20] [3]
-- [10] [2,3]
-- [] [1,2,3]
-- ["1"] [2,3]
-- ["1","2"] [3]
-- ["1","2","3"] []
-- ["1","2","3"]


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
