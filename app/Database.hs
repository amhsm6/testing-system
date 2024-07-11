{-# LANGUAGE DeriveGeneric #-}

module Database where

import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics
import System.Environment
import Servant
import Database.HDBC
import Database.HDBC.PostgreSQL

type DB = ReaderT Connection Handler

runDB :: DB a -> Handler a
runDB m = do
    conn <- liftIO $ do
        host <- getEnv "DBHOST"
        user <- getEnv "DBUSER"
        pass <- getEnv "DBPASS"
        db   <- getEnv "DBNAME"
        connectPostgreSQL $ concat [ "host = ", host, " "
                                   , "user = ", user, " "
                                   , "password = ", pass, " "
                                   , "dbname = ", db
                                   ]

    runReaderT m conn

data Course = Course { courseId :: Int
                     , name :: String
                     }
                     deriving Generic

instance FromJSON Course
instance ToJSON Course

data Problem = Problem { problemId :: Int
                       , description :: String
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
                            [("course_id", id), ("name", name)] -> pure $ Course (fromSql id) (fromSql name)
                            _ -> throwError err500
