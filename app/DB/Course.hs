{-# LANGUAGE TemplateHaskell #-}

module DB.Course where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Servant
import Database.HDBC

import DB
import DB.Problem

data Course = Course { __courseId :: Int
                     , __name :: String
                     , __problems :: [Problem]
                     }

makeLenses ''Course

getCourses :: DB [Course]
getCourses = do
    c <- ask
    rows <- liftIO $ do
        st <- prepare c "SELECT course_id, name FROM courses ORDER BY course_id"
        execute st []
        fetchAllRows st

    forM rows $ \row -> case row of
                            [courseId, name] -> pure $ Course (fromSql courseId) (fromSql name) []
                            _ -> throwError err500

getCourse :: Int -> DB (Maybe Course)
getCourse courseId = do
    c <- ask
    row <- liftIO $ do
        st <- prepare c "SELECT name FROM courses WHERE course_id = ?"
        execute st [toSql courseId]
        fetchRow st

    problems <- getProblems courseId

    case row of
        Just [name] -> pure $ Just $ Course courseId (fromSql name) problems
        Nothing -> pure Nothing
        _ -> throwError err500
