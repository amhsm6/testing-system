{-# LANGUAGE DeriveGeneric #-}

module Api.Course where

import Control.Monad
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import DB.Course

data GetCourses = GetCourses { courses :: [GetCourse] }
    deriving Generic

data GetCourse = GetCourse { courseId :: Int
                           , courseName :: String
                           }
                           deriving Generic

instance FromJSON GetCourse
instance ToJSON GetCourse

instance FromJSON GetCourses
instance ToJSON GetCourses
