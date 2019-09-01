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
--       putStr $ show (o ^.. clowns)
--       putStr " "
--       putStr $ show (o ^. present)
--       putStr " "
--       putStr $ show (o ^.. jokers)
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


-- | Given a partially-executed 'OracleMachine' computation, we are interested
-- in exploring alternative executions by varying one answer and then a fraction
-- of the following questions. To do that, we remember all the answers we have
-- received so far, and for each of those, we have a 'Suspended' computation
-- which would allow us to explore what would have happened if the answer had
-- been different.
--
-- If we are currently exploring such an alternative path, we may also have a
-- list of replacements for the questions to come.
data OracleBookkeeping c a j r = OracleBookkeeping
  { _past    :: Seq (Suspended c c j r)
  , _present :: a
  , _future  :: [j]
  }

makeLenses ''OracleBookkeeping


empty
  :: OracleBookkeeping c () j r
empty
  = OracleBookkeeping mempty () mempty

right
  :: (a -> Maybe j -> (Suspended c c j r, b))
  -> OracleBookkeeping c a j r
  -> OracleBookkeeping c b j r
right f o
  = OracleBookkeeping past' b future'
  where
    a       = o ^. present
    maybeJ  = firstOf (future . each) o
    (s, b)  = f a maybeJ
    past'   = (o ^. past) |> s
    future' = drop 1 (o ^. future)

left
  :: (Suspended c c j r -> a -> (b, j))
  -> OracleBookkeeping c a j r
  -> Maybe (OracleBookkeeping c b j r)
left f o = do
  past' :> s <- pure (o ^. past)
  let a       = o ^. present
  let (b, j)  = f s a
  let future' = j : o ^. future
  pure $ OracleBookkeeping past' b future'


clowns
  :: Traversal' (OracleBookkeeping c a j r) c
clowns
  = past . each . thoughts

jokers
  :: Traversal' (OracleBookkeeping c a j r) j
jokers
  = future . each
