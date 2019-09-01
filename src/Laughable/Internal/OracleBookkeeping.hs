{-# LANGUAGE TemplateHaskell #-}
module Laughable.Internal.OracleBookkeeping where

import Control.Lens
import Data.Sequence (Seq)

import Laughable.Internal.OracleMachine

-- $setup
-- >>> :{
-- let test :: Show a
--          => OracleBookkeeping Int a Int () -> IO ()
--     test o = do
--       putStr $ show (o ^.. past . each . thoughts)
--       putStr " "
--       putStr $ show (o ^. present)
--       putStr " "
--       putStr $ show (o ^. future)
--       putStrLn ""
-- :}
--
-- >>> let oracleMachine = mapM_ askOracle [1,2,3]
-- >>> let Left s1 = runOracleMachine oracleMachine
-- >>> let Left s2 = runOracleMachine $ resumeWithThoughts s1
-- >>> let Left s3 = runOracleMachine $ resumeWithThoughts s2
--
-- >>> let b0       = empty & present .~ 'a'
-- >>> let b1       = b0    & right (\a Nothing -> (s1, succ a))
-- >>> let b2       = b1    & right (\a Nothing -> (s2, succ a))
-- >>> let b3       = b2    & right (\a Nothing -> (s3, succ a))
-- >>> let Just b2' = b3    & left (\s a -> (succ a, s ^. thoughts))
-- >>> let Just b1' = b2'   & left (\s a -> (succ a, s ^. thoughts))
-- >>> let Just b0' = b1'   & left (\s a -> (succ a, s ^. thoughts))
-- >>> let b1''     = b0'   & right (\a (Just 1) -> (s1, succ a))
-- >>> let b2''     = b1''  & right (\a (Just 2) -> (s2, succ a))
-- >>> let b3''     = b2''  & right (\a (Just 3) -> (s3, succ a))
-- >>> mapM_ test [b0, b1, b2, b3, b2', b1', b0', b1'', b2'', b3'']
-- [] 'a' []
-- [1] 'b' []
-- [1,2] 'c' []
-- [1,2,3] 'd' []
-- [1,2] 'e' [3]
-- [1] 'f' [2,3]
-- [] 'g' [1,2,3]
-- [1] 'h' [2,3]
-- [1,2] 'i' [3]
-- [1,2,3] 'j' []


-- | Given a partially-executed OracleMachine computation, we keep track of the
-- questions it has already asked to the oracle, the answers it has received,
-- and most importantly a snapshot of the computation at the moment it asked
-- the question, so we can investigate what would have happened if the answer
-- had been different. Not that the oracle has ever been wrong.
--
-- If we are currently exploring such an alternative path, we may also know
-- some of the future answers in advance, assuming that the questions will be
-- the same.
data OracleBookkeeping c a j r = OracleBookkeeping
  { _past    :: Seq (Suspended c j j r)
  , _present :: a
  , _future  :: [c]
  }

makeLenses ''OracleBookkeeping


empty
  :: OracleBookkeeping c () j r
empty
  = OracleBookkeeping mempty () mempty

right
  :: (a -> Maybe c -> (Suspended c j j r, b))
  -> OracleBookkeeping c a j r
  -> OracleBookkeeping c b j r
right f o
  = OracleBookkeeping past' b future'
  where
    a       = o ^. present
    maybeC  = firstOf (future . each) o
    (s, b)  = f a maybeC
    past'   = (o ^. past) |> s
    future' = drop 1 (o ^. future)

left
  :: (Suspended c j j r -> a -> (b, c))
  -> OracleBookkeeping c a j r
  -> Maybe (OracleBookkeeping c b j r)
left f o = do
  past' :> s <- pure (o ^. past)
  let a       = o ^. present
  let (b, c)  = f s a
  let future' = c : o ^. future
  pure $ OracleBookkeeping past' b future'
