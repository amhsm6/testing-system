{-# LANGUAGE TemplateHaskell #-}

module DB.Problem where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Servant
import Database.HDBC

import DB

data Problem = Problem { __problemId :: Int
                       , __description :: String
                       }

makeLenses ''Problem

getProblems :: Int -> DB [Problem]
getProblems courseId = do
    c <- ask
    rows <- liftIO $ do
        st <- prepare c "SELECT problem_id, description FROM problems WHERE course_id = ? ORDER BY problem_id"
        execute st [toSql courseId]
        fetchAllRows st

    forM rows $ \row -> case row of
                            [problemId, desc] -> pure $ Problem (fromSql problemId) (fromSql desc)
                            _ -> throwError err500

getProblem :: Int -> DB (Maybe Problem)
getProblem problemId = do
    c <- ask
    row <- liftIO $ do
        st <- prepare c "SELECT description FROM problems WHERE problem_id = ?"
        execute st [toSql problemId]
        fetchRow st

    case row of
        Just [desc] -> pure $ Just $ Problem problemId (fromSql desc)
        Nothing -> pure Nothing
        _ -> throwError err500
