{-# LANGUAGE DeriveFunctor, LambdaCase, TemplateHaskell #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Laughable.Internal.OracleMachine where

import Control.Lens
import Control.Monad.Trans.State

-- $setup
-- >>> let example = mapM askOracle ["foo", "bar", "baz"]


-- | A computation which computes an 'a' by asking an oracle questions of type
-- 'j' and receiving answers of type 'c'.
data OracleMachine c j r
  = Nil r
  | Cons (Suspended c () j r)
  deriving Functor

-- | An OracleMachine computation, suspended while the oracle thinks about the
-- answer. Her current thoughts have type 'a', but we can only proceed once she
-- has shaped them into a 'c'.
data Suspended c a j r = Suspended
  { _question     :: j
  , _answer       :: a
  , _continuation :: c -> OracleMachine c j r
  }
  deriving Functor

makePrisms ''OracleMachine
makeLenses ''Suspended


resumeWithQuestion
  :: Suspended a x a r
  -> OracleMachine a a r
resumeWithQuestion s
  = (s ^. continuation) (s ^. question)

resumeWithAnswer
  :: Suspended c c j r
  -> OracleMachine c j r
resumeWithAnswer s
  = (s ^. continuation) (s ^. answer)

resumeWithValue
  :: c
  -> Suspended c a j r
  -> OracleMachine c j r
resumeWithValue c s
  = (s ^. continuation) c


-- |
-- >>> let Left s1 = runOracleMachine example
-- >>> let Left s2 = runOracleMachine $ resumeWithQuestion s1
-- >>> let Left s3 = runOracleMachine $ resumeWithAnswer (s2 & answer %~ show)
-- >>> let Right r = runOracleMachine $ resumeWithValue "mu" s3
-- >>> r
-- ["foo","()","mu"]
runOracleMachine
  :: OracleMachine c j r
  -> Either (Suspended c () j r) r
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
    let j = s ^. question
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
  = Cons $ Suspended j () pure


instance Applicative (OracleMachine a b) where
  pure = Nil
  Nil f <*> lr
    = f <$> lr
  Cons (Suspended a () lf) <*> lr
    = Cons $ Suspended a () $ \b
   -> lf b <*> lr

instance Monad (OracleMachine a b) where
  Nil r >>= cc
    = cc r
  Cons (Suspended a () lf) >>= cc
    = Cons $ Suspended a () $ \b
   -> lf b >>= cc
