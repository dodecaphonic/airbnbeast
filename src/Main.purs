module Main
  ( main
  ) where

import Prelude

import Airbnbeast.Storage as Storage
import Airbnbeast.WebServer as WebServer
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Effect.Class (liftEffect)
import Node.Process as Process
import SQLite3 as SQLite3

openDBConnection :: Aff SQLite3.DBConnection
openDBConnection = do
  dbPath <- liftEffect $ fromMaybe "db/airbnbeast.sqlite3" <$> Process.lookupEnv "DATABASE_PATH"
  SQLite3.newDB dbPath

main :: Effect (Fiber (Effect Unit -> Effect Unit))
main = launchAff do
  storage <- Storage.sqliteStorage <$> openDBConnection
  liftEffect $ WebServer.startServer { storage, port: 8080 }

