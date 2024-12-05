module Hasql.Transaction.Private.Transaction where

import Hasql.Session qualified as B
import Hasql.Statement qualified as A
import Hasql.Transaction.Config
import Hasql.Transaction.Private.Prelude
import Hasql.Transaction.Private.Sessions qualified as D

-- |
-- A composable abstraction over the retryable transactions.
--
-- Executes multiple queries under the specified mode and isolation level,
-- while automatically retrying the transaction in case of conflicts.
-- Thus this abstraction closely reproduces the behaviour of 'STM'.
newtype Transaction error a
  = Transaction (StateT (Maybe error) B.Session a)
  deriving (Functor, Applicative, Monad)

instance (Semigroup a) => Semigroup (Transaction error a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (Transaction error a) where
  mempty = pure mempty

-- |
-- Execute the transaction using the provided isolation level and mode.
{-# INLINE run #-}
run :: Transaction error a -> IsolationLevel -> Mode -> Bool -> B.Session (Either error a)
run (Transaction session) isolation mode preparable =
  D.inRetryingTransaction isolation mode (runStateT session Nothing) preparable

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
{-# INLINE sql #-}
sql :: ByteString -> Transaction error ()
sql =
  Transaction . lift . B.sql

-- |
-- Parameters and a specification of the parametric query to apply them to.
{-# INLINE statement #-}
statement :: a -> A.Statement a b -> Transaction error b
statement params statement =
  Transaction . lift $ B.statement params statement

-- |
-- Cause transaction to eventually roll back.
{-# INLINE condemn #-}
condemn :: error -> Transaction error ()
condemn err =
  Transaction $ put $ Just err
