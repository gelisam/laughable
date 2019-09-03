{-# OPTIONS -Wno-name-shadowing #-}
module Laughable.Dissected where

import Control.Lens

import Laughable.Internal.Types
import Laughable.Internal.DissectedList (DissectedList(..))
import Laughable.Internal.SplitTraversable (SplitTraversable(..), replaceValues)
import qualified Laughable.Internal.DissectedList as DissectedList
import qualified Laughable.Internal.SplitTraversable as SplitTraversable

-- $setup
-- >>> :{
-- let printDissected
--       :: (Show c, Show a, Show j)
--       => Dissected t c a j
--       -> IO ()
--     printDissected d = do
--       putStr $ show (d ^.. clowns)
--       putStr " "
--       putStr $ show (d ^. focus)
--       putStr " "
--       putStr $ show (d ^.. jokers)
--       putStrLn ""
--     testTransform
--       :: (Show c, Show b, Show j)
--       => (a -> Dissected t c b j)
--       -> a -> IO (Dissected t c b j)
--     testTransform f a = do
--       let d = f a
--       printDissected d
--       pure d
--     testLeft
--       :: (Show c, Show j)
--       => Dissected t c j j
--       -> IO (Dissected t c c j)
--     testLeft d = do
--       case left d of
--         Left _ -> do
--           error "testLeft: left thinks we're at the leftmost position"
--         Right d -> do
--           printDissected d
--           pure d
--     testRight
--       :: (Show c, Show j)
--       => Dissected t c c j
--       -> IO (Dissected t c j j)
--     testRight d = do
--       case right d of
--         Left _ -> do
--           error "testRight: right thinks we're at the rightmost position"
--         Right d -> do
--           printDissected d
--           pure d
--     testLeftmost
--       :: Dissected t c a j
--       -> IO (Dissected t void a j)
--     testLeftmost d = do
--       case left (d & focus .~ undefined) of
--         Left d' -> do
--           pure $ d' & focus .~ (d ^. focus)
--         Right _ -> do
--           error "testLeftmost: left does not think we're at the leftmost position"
--     testRightmost
--       :: Dissected t c a j
--       -> IO (Dissected t c a void)
--     testRightmost d = do
--       case right (d & focus .~ undefined) of
--         Left d' -> do
--           pure $ d' & focus .~ (d ^. focus)
--         Right _ -> do
--           error "testRightmost: right does not think we're at the rightmost position"
-- :}
--
-- >>> import Control.Monad
-- >>> import Data.Either
-- >>> import qualified Laughable.Traversable as Traversable
-- >>> :{
-- fromTraversable [1,2,3] & ( testTransform (fromRight undefined)
--                         >=> testLeftmost
--                         >=> testTransform (focus %~ (* 10))
--                         >=> testRight
--                         >=> testTransform (focus %~ (* 10))
--                         >=> testRight
--                         >=> testTransform (focus %~ (* 10))
--                         >=> testRightmost
--                         >=> testTransform (focus %~ (`div` 10))
--                         >=> testLeft
--                         >=> testTransform (focus %~ (`div` 10))
--                         >=> testLeft
--                         >=> testTransform (focus %~ (`div` 10))
--                         >=> testLeftmost
--                         >=> testTransform (focus %~ show)
--                         >=> testRight
--                         >=> testTransform (focus %~ show)
--                         >=> testRight
--                         >=> testTransform (focus %~ show)
--                         >=> testRightmost
--                         >=> pure . Traversable.fromDissected
--                           )
-- :}
-- [] 1 [2,3]
-- [] 10 [2,3]
-- [10] 2 [3]
-- [10] 20 [3]
-- [10,20] 3 []
-- [10,20] 30 []
-- [10,20] 3 []
-- [10] 20 [3]
-- [10] 2 [3]
-- [] 10 [2,3]
-- [] 1 [2,3]
-- [] "1" [2,3]
-- ["1"] 2 [3]
-- ["1"] "2" [3]
-- ["1","2"] 3 []
-- ["1","2"] "3" []
-- ["1","2","3"]


fromTraversable
  :: Traversable t
  => t j
  -> Either (t void) (Dissected t c j j)
fromTraversable tj
  = let s = SplitTraversable.fromTraversable tj
    in case traverseOf SplitTraversable.values DissectedList.climbRight s of
         Left _
           -> case replaceValues (s ^. SplitTraversable.shape) [] of
                Just tv
                  -> Left tv
                Nothing
                  -> error "Dissected.fromBisected: never happens, the Traversable is clearly empty"
         Right s
           -> Right $ Dissected $ s


halfLeft
  :: Dissected t c j j
  -> Bisected t c j
halfLeft (Dissected (SplitTraversable tu vs))
  = Bisected $ SplitTraversable tu $ DissectedList.slideLeft vs

halfRight
  :: Dissected t c c j
  -> Bisected t c j
halfRight (Dissected (SplitTraversable tu vs))
  = Bisected $ SplitTraversable tu $ DissectedList.slideRight vs

left
  :: Dissected t c j j
  -> Either (Dissected t void j j)
            (Dissected t c c j)
left (Dissected (SplitTraversable tu vs))
  = case DissectedList.climbLeft $ DissectedList.slideLeft vs of
      Left _
        -> Left $ Dissected $ SplitTraversable tu
         $ vs & DissectedList.clownSeq .~ mempty
      Right vs
        -> Right $ Dissected $ SplitTraversable tu vs

right
  :: Dissected t c c j
  -> Either (Dissected t c c void)
            (Dissected t c j j)
right (Dissected (SplitTraversable tu vs))
  = case DissectedList.climbRight $ DissectedList.slideRight vs of
      Left _
        -> Left $ Dissected $ SplitTraversable tu
         $ vs & DissectedList.jokerList .~ mempty
      Right vs
        -> Right $ Dissected $ SplitTraversable tu vs


clowns
  :: Traversal (Dissected t c a j) (Dissected t c' a j) c c'
clowns
  = _Dissected . SplitTraversable.values . DissectedList.clowns

focus
  :: Lens (Dissected t c a j) (Dissected t c b j) a b
focus
  = _Dissected . SplitTraversable.values . DissectedList.focus

jokers
  :: Traversal (Dissected t c a j) (Dissected t c a j') j j'
jokers
  = _Dissected . SplitTraversable.values . DissectedList.jokers

elements
  :: Traversal (Dissected t a a a) (Dissected t b b b) a b
elements f (Dissected (SplitTraversable tu (DissectedList cs a js)))
    = Dissected
  <$> SplitTraversable tu
  <$> (DissectedList <$> traverse f cs <*> f a <*> traverse f js)
