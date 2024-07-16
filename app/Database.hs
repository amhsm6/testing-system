{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Data.ByteString.Lens
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

data Course = Course { __courseId :: Int
                     , __name :: String
                     , __problems :: [Problem]
                     }
                     deriving Generic

data Problem = Problem { __problemId :: Int
                       , __description :: String
                       }
                       deriving Generic

instance FromJSON Problem
instance ToJSON Problem

instance FromJSON Course
instance ToJSON Course

makeLenses ''Course
makeLenses ''Problem

getCourses :: DB [Course]
getCourses = do
    c <- ask
    rows <- liftIO $ do
        st <- prepare c "SELECT * FROM courses"
        execute st []
        fetchAllRows st

    forM rows $ \row -> case row of
                            [id, name] -> pure $ Course (fromSql id) (fromSql name) []
                            _ -> throwError $ err500 { errBody = Chars "Unknown Error" }

getCourse :: Int -> DB Course
getCourse id = do
    c <- ask

    row <- liftIO $ do
        st <- prepare c "SELECT * FROM courses WHERE course_id = ?"
        execute st [toSql id]
        fetchRow st

    course <- case row of
                  Just [id, name] -> pure $ Course (fromSql id) (fromSql name) []
                  _ -> throwError $ err500 { errBody = Chars "Unknown Error" }

    rows <- liftIO $ do
        st <- prepare c "SELECT * FROM problems WHERE course_id = ? ORDER BY problem_id"
        execute st [toSql id]
        fetchAllRows st

    problems <- forM rows $ \row -> case row of
                                        [id, desc, _] -> pure $ Problem (fromSql id) (fromSql desc)
                                        _ -> throwError $ err500 { errBody = Chars "Unknown Error" }

    pure $ course & _problems .~ problems
