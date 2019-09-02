{-# LANGUAGE DeriveFunctor, LambdaCase, TemplateHaskell #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Laughable.Internal.OracleMachine where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

-- $setup
-- >>> let example = mapM askOracle ["foo", "bar", "baz"]


-- | A computation which computes an 'r' by asking an oracle questions of type
-- 'j' and receiving answers of type 'c'.
data OracleMachine c j r
  = Nil r
  | Cons (Suspended c j j r)
  deriving Functor

-- | An OracleMachine computation, suspended while the oracle focusses her
-- thoughts on the question, of type 'j'. Her focus may then wander and have
-- other types, but we can only proceed once she focusses on an answer, of type
-- 'c'.
data Suspended c a j r = Suspended
  { _focus        :: a
  , _continuation :: c -> OracleMachine c j r
  }
  deriving Functor

makePrisms ''OracleMachine
makeLenses ''Suspended


resumeWithFocus
  :: Suspended c c j r
  -> OracleMachine c j r
resumeWithFocus s
  = (s ^. continuation) (s ^. focus)

resumeWithValue
  :: c
  -> Suspended c a j r
  -> OracleMachine c j r
resumeWithValue c s
  = (s ^. continuation) c


-- |
-- >>> let Left s1 = runOracleMachine example
-- >>> let Left s2 = runOracleMachine $ resumeWithFocus s1
-- >>> let Left s3 = runOracleMachine $ resumeWithValue "mu" s2
-- >>> let Right r = runOracleMachine $ resumeWithFocus s3
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
    let j = s ^. focus
    c <- act j
    runOracleMachineWithAction act $ resumeWithValue c s

-- |
-- >>> runOracleMachineWithAnswers ['a', 'b'] example
-- Nothing
-- >>> runOracleMachineWithAnswers ['a', 'b', 'c'] example
-- Just "abc"
-- >>> runOracleMachineWithAnswers ['a', 'b', 'c', 'd'] example
-- Just "abc"
runOracleMachineWithAnswers
  :: [c]
  -> OracleMachine c j r
  -> Maybe r
runOracleMachineWithAnswers as o
  = runIdentity
  $ runMaybeT
  $ flip evalStateT as
  $ flip runOracleMachineWithAction o
  $ \_ -> do
      get >>= \case
        [] -> do
          empty
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
