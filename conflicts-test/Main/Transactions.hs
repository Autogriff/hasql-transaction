module Main.Transactions where

import Hasql.Transaction
import Main.Statements qualified as A
import Data.Void (Void)
import Prelude

createSchema :: Transaction Void ()
createSchema =
  do
    statement () A.createAccountTable

dropSchema :: Transaction Void ()
dropSchema =
  do
    statement () A.dropAccountTable

transfer :: Int64 -> Int64 -> Scientific -> Transaction Void Bool
transfer id1 id2 amount =
  do
    success <- statement (id1, amount) A.modifyBalance
    if success
      then statement (id2, negate amount) A.modifyBalance
      else return False

transferTimes :: Int -> Int64 -> Int64 -> Scientific -> Transaction Void ()
transferTimes times id1 id2 amount =
  replicateM_ times (transfer id1 id2 amount)
