{-# LANGUAGE DeriveFunctor, LambdaCase, TemplateHaskell #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Laughable.Internal.OracleMachine where

import Control.Lens
import Control.Monad.Trans.State

-- $setup
-- >>> let example = mapM askOracle ["foo", "bar", "baz"]


-- | A computation which computes an 'r' by asking an oracle questions of type
-- 'j' and receiving answers of type 'c'.
data OracleMachine c j r
  = Nil r
  | Cons (Suspended c j j r)
  deriving Functor

-- | An OracleMachine computation, suspended while the oracle thinks about the
-- answer. At first, her thoughts contain the question, of type 'j', but we can
-- only proceed once she has shaped them into a 'c'.
data Suspended c a j r = Suspended
  { _thoughts     :: a
  , _continuation :: c -> OracleMachine c j r
  }
  deriving Functor

makePrisms ''OracleMachine
makeLenses ''Suspended


resumeWithThoughts
  :: Suspended c c j r
  -> OracleMachine c j r
resumeWithThoughts s
  = (s ^. continuation) (s ^. thoughts)

resumeWithValue
  :: c
  -> Suspended c a j r
  -> OracleMachine c j r
resumeWithValue c s
  = (s ^. continuation) c


-- |
-- >>> let Left s1 = runOracleMachine example
-- >>> let Left s2 = runOracleMachine $ resumeWithThoughts s1
-- >>> let Left s3 = runOracleMachine $ resumeWithValue "mu" s2
-- >>> let Right r = runOracleMachine $ resumeWithThoughts s3
-- >>> r
-- ["foo","mu","baz"]
runOracleMachine
  :: OracleMachine c j r
  -> Either (Suspended c j j r) r
runOracleMachine (Nil r)
  = Right r
runOracleMachine (Cons s)
  = Left s

-- |
-- >>> runOracleMachineWithAction print example
-- "foo"
-- "bar"
-- "baz"
-- [(),(),()]
runOracleMachineWithAction
  :: Monad m
  => (j -> m c)
  -> OracleMachine c j r -> m r
runOracleMachineWithAction act = \case
  Nil r -> do
    pure r
  Cons s -> do
    let j = s ^. thoughts
    c <- act j
    runOracleMachineWithAction act $ resumeWithValue c s

-- |
-- >>> runOracleMachineWithReplacements ["fool", "bard"] example
-- ["fool","bard","baz"]
runOracleMachineWithReplacements
  :: [a]
  -> OracleMachine a a r -> r
runOracleMachineWithReplacements as o
  = flip evalState as
  $ flip runOracleMachineWithAction o
  $ \a -> do
      get >>= \case
        [] -> do
          pure a
        (a:as) -> do
          put as
          pure a

-- |
-- >>> runOracleMachineWithFunction (++ "!") example
-- ["foo!","bar!","baz!"]
runOracleMachineWithFunction
  :: (j -> c)
  -> OracleMachine c j r -> r
runOracleMachineWithFunction f
  = runIdentity . runOracleMachineWithAction (pure . f)

-- |
-- >>> runOracleMachineWithIdentity example
-- ["foo","bar","baz"]
runOracleMachineWithIdentity
  :: OracleMachine a a r -> r
runOracleMachineWithIdentity
  = runOracleMachineWithFunction id


askOracle
  :: j -> OracleMachine c j c
askOracle j
  = Cons $ Suspended j pure


instance Applicative (OracleMachine c j) where
  pure = Nil
  Nil f <*> lr
    = f <$> lr
  Cons (Suspended j lf) <*> lr
    = Cons $ Suspended j $ \c
   -> lf c <*> lr

instance Monad (OracleMachine c j) where
  Nil r >>= cc
    = cc r
  Cons (Suspended j lf) >>= cc
    = Cons $ Suspended j $ \c
   -> lf c >>= cc
