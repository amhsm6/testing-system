{-# LANGUAGE DeriveGeneric #-}

module Api.Course.CoursesResp where

import Control.Monad
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import DB.Course

data CoursesResp = CoursesResp { courses :: [CoursesRespCourse] }
    deriving Generic

data CoursesRespCourse = CoursesRespCourse { id :: Int
                                           , name :: String
                                           }
                                           deriving Generic

instance FromJSON CoursesRespCourse
instance ToJSON CoursesRespCourse

instance FromJSON CoursesResp
instance ToJSON CoursesResp

coursesResp :: Getter [Course] CoursesResp
coursesResp = lens mapCourses const
    where mapCourses = CoursesResp . map mapCourse
          mapCourse course = CoursesRespCourse (course ^. _courseId) (course ^. _name)
