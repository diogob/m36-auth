module Main where

import ProjectM36.Client

import Protolude
import Lib

main :: IO ()
main = do
  --connect to the database
  let connInfo = InProcessConnectionInfo NoPersistence emptyNotificationCallback []
      check x = case x of
        Left err -> error (show err)
        Right x' -> x'
  eConn <- connectProjectM36 connInfo
  let conn = check eConn

  --create a database session at the default branch of the fresh database
  eSessionId <- createSessionAtHead conn "master"
  let sessionId = check eSessionId

  createSchema sessionId conn
  insertSampleData sessionId conn
