{-# LANGUAGE DeriveGeneric #-}

module Database where

import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics
import Servant
import Database.HDBC
import Database.HDBC.Sqlite3

type DB = ReaderT Connection Handler

runDB :: DB a -> Handler a
runDB m = liftIO (connectSqlite3 "db.sqlite") >>= runReaderT m

data Course = Course { courseId :: Int
                     , name :: String
                     }
                     deriving Generic

instance FromJSON Course
instance ToJSON Course

data Problem = Problem { problemId :: Int
                       , desc :: String
                       }
                       deriving Generic

instance FromJSON Problem
instance ToJSON Problem

getCourses :: DB [Course]
getCourses = do
    c <- ask
    rows <- liftIO $ do
        st <- prepare c "SELECT * FROM courses"
        execute st []
        fetchAllRowsAL st

    forM rows $ \row -> case row of
                            [("id", id), ("name", name)] -> pure $ Course (fromSql id) (fromSql name)
                            _ -> throwError err500
