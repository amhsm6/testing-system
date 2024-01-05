{-# LANGUAGE DeriveGeneric #-}

module Database where

import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics
import Servant
import Database.HDBC
import Database.HDBC.Sqlite3

import Tester

type DB = ReaderT Connection Handler

runDB :: DB a -> Handler a
runDB db = liftIO (connectSqlite3 "db.sqlite") >>= runReaderT db

data Problem = Problem { problemId :: Int
                       , courseId :: Int
                       , test :: Test
                       }
                       deriving Generic

instance FromJSON Problem
instance ToJSON Problem

data Course = Course { id :: Int
                     }
                     deriving Generic

instance FromJSON Course
instance ToJSON Course

type Courses = [Course]

courses = undefined

course = undefined

problem = undefined
