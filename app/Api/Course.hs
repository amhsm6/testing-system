{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Course where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
import Database.HDBC

import DB

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

data Test = Test { __testId :: Int
                 , __input :: String
                 , __output :: String
                 }
                 deriving Generic

instance FromJSON Test
instance ToJSON Test

makeLenses ''Test

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

    course <- case row of
                  Just [name] -> pure $ Just $ Course courseId (fromSql name) []
                  Nothing -> pure Nothing
                  _ -> throwError err500

    rows <- liftIO $ do
        st <- prepare c "SELECT problem_id, description FROM problems WHERE course_id = ? ORDER BY problem_id"
        execute st [toSql courseId]
        fetchAllRows st

    problems <- forM rows $ \row -> case row of
                                        [problemId, desc] -> pure $ Problem (fromSql problemId) (fromSql desc)
                                        _ -> throwError err500

    pure $ course & _Just . _problems .~ problems

checkProblem :: Int -> DB Bool
checkProblem problemId = do
    c <- ask
    liftIO $ do
        st <- prepare c "SELECT * FROM problems WHERE problem_id = ?"
        execute st [toSql problemId]
        has _Just <$> fetchRow st

getTests :: Int -> DB [Test]
getTests problemId = do
    c <- ask
    rows <- liftIO $ do
        st <- prepare c "SELECT test_id, input, output FROM tests WHERE problem_id = ? ORDER BY test_id"
        execute st [toSql problemId]
        fetchAllRows st

    forM rows $ \row -> case row of
                            [testId, input, output] -> pure $ Test (fromSql testId) (fromSql input) (fromSql output)
                            _ -> throwError err500
