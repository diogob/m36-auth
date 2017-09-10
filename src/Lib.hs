{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
-- the Out-of-the-Tarpit example in Haskell and Project:M36
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}
module Lib
    ( createSchema
    , insertSampleData
    ) where

import ProjectM36.Client
import ProjectM36.DataTypes.Primitive
import ProjectM36.Tupleable
import ProjectM36.Relation
import ProjectM36.Error
import Data.Either
import GHC.Generics
import Data.Binary
import Control.DeepSeq
import qualified Data.Text as T
import Data.Time.Calendar

import Lib.Prelude

--create various database value (atom) types
data AccountState = Active | NoPassword | InactiveSince Day
               deriving (Generic, Atomable, Eq, Show, Binary, NFData)

data User = User {
  name :: Text,
  email :: Text,
  address :: Text,
  accountState :: AccountState,
  dateRegistered :: Day
  } deriving (Generic, Eq, Show)

instance Tupleable User

createSchema :: SessionId -> Connection -> IO ()
createSchema sessionId conn = do
  --create attributes for relvars
  let
      --create uniqueness constraints
      incDepKeys = map (uncurry databaseContextExprForUniqueKey) [("user", ["email"])]

      --define the relvars
      rvExprs = [toDefineExpr (undefined :: User) "user"]
      --create the new algebraic data types
      new_adts = [toAddTypeExpr (undefined :: AccountState)]

  --gather up and execute all database updates
  putStrLn ("load relvars" :: Text)
  _ <- handleIOErrors $ mapM (executeDatabaseContextExpr sessionId conn) (new_adts ++ rvExprs ++ incDepKeys)
  pure ()

insertSampleData :: SessionId -> Connection ->  IO ()
insertSampleData sessionId conn = do
  --insert a bunch of records
  putStrLn ("load data" :: Text)
  let properties = [ User { name = "Active User"
                          , email = "active@email.com"
                          , address = "123 Main St."
                          , accountState = Active
                          , dateRegistered = fromGregorian 2016 4 3}
                   , User { name = "Inactive User"
                          , email = "inactive@email.com"
                          , address = "123 Main St."
                          , accountState = InactiveSince $ fromGregorian 2017 4 3
                          , dateRegistered = fromGregorian 2016 4 3}
                  ]
  insertPropertiesExpr <- handleError $ toInsertExpr properties "user"
  handleIOError $ executeDatabaseContextExpr sessionId conn insertPropertiesExpr

  --query some records, marshal them back to Haskell

  properties' <- handleIOError $ executeRelationalExpr sessionId conn (RelationVariable "user" ())

  props <- toList properties' >>= mapM (handleError . fromTuple) :: IO [User]
  print props

handleError :: Either RelationalError a -> IO a
handleError eErr = case eErr of
    Left err -> print err >> panic "Died due to errors."
    Right v -> pure v

handleIOError :: IO (Either RelationalError a) -> IO a
handleIOError m = do
  e <- m
  handleError e

handleIOErrors :: IO [Either RelationalError a] -> IO [a]
handleIOErrors m = do
  eErrs <- m
  case lefts eErrs of
    [] -> pure (rights eErrs)
    errs -> handleError (Left (someErrors errs))
