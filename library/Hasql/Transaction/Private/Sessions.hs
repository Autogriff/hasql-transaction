module Hasql.Transaction.Private.Sessions where

import Hasql.Session
import Hasql.Transaction.Config
import Hasql.Transaction.Private.Prelude
import Hasql.Transaction.Private.Statements qualified as Statements

{-
We may want to do one transaction retry in case of the 23505 error, and fail if an identical
error is seen.
-}
inRetryingTransaction :: IsolationLevel -> Mode -> Session (a, Maybe e) -> Bool -> Session (Either e a)
inRetryingTransaction level mode session preparable =
  fix $ \retry -> do
    attemptRes <- tryTransaction level mode session preparable
    case attemptRes of
      Just (Right a) -> return $ Right a  -- Transaction succeeded
      Just (Left e) -> return $ Left e    -- User error occurred
      Nothing -> retry                    -- Retryable transaction error occurred

tryTransaction :: IsolationLevel -> Mode -> Session (a, Maybe e) -> Bool -> Session (Maybe (Either e a))
tryTransaction level mode body preparable = do
  statement () (Statements.beginTransaction level mode preparable)

  bodyRes <- catchError (fmap Just body) $ \error -> do
    statement () (Statements.abortTransaction preparable)
    handleTransactionError error $ return Nothing

  case bodyRes of
    Just (val, maybeErr) -> catchError (commitOrAbort maybeErr preparable $> Just (maybe (Right val) Left maybeErr)) $ \error -> do
      handleTransactionError error $ return Nothing
    Nothing -> return Nothing

commitOrAbort :: Maybe e -> Bool -> Session ()
commitOrAbort maybeErr preparable =
  case maybeErr of
    Nothing -> statement () (Statements.commitTransaction preparable)
    Just _ -> statement () (Statements.abortTransaction preparable)

handleTransactionError :: SessionError -> Session a -> Session a
handleTransactionError error onTransactionError = case error of
  QueryError _ _ clientError -> onCommandError clientError
  PipelineError clientError -> onCommandError clientError
  where
    onCommandError = \case
      ResultError (ServerError code _ _ _ _) ->
        case code of
          "40001" -> onTransactionError
          "40P01" -> onTransactionError
          _ -> throwError error
      _ -> throwError error