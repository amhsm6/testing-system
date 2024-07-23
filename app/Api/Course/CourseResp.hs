{-# LANGUAGE DeriveGeneric #-}

module Api.Course.CourseResp where

import Control.Monad
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import DB.Course
import DB.Problem

data CourseResp = CourseResp { name :: String
                             , problems :: [CourseRespProblem]
                             }
                             deriving Generic

data CourseRespProblem = CourseRespProblem { id :: Int
                                           , description :: String
                                           }
                                           deriving Generic

instance FromJSON CourseRespProblem
instance ToJSON CourseRespProblem

instance FromJSON CourseResp
instance ToJSON CourseResp

courseResp :: Getter (Maybe Course) (Maybe CourseResp)
courseResp = lens (fmap mapCourse) const
    where mapCourse course = CourseResp (course ^. _name) (course ^.. _problems . traverse . to mapProblem)
          mapProblem problem = CourseRespProblem (problem ^. _problemId) (problem ^. _description)
