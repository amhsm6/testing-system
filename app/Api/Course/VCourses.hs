{-# LANGUAGE DeriveGeneric #-}

module Api.Course.VCourses
    ( VCourses, vcourses
    ) where

import Control.Monad
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import DB.Course

data VCourses = VCourses { courses :: [VCourse] }
    deriving Generic

data VCourse = VCourse { id :: Int
                       , name :: String
                       }
                       deriving Generic

instance FromJSON VCourse
instance ToJSON VCourse

instance FromJSON VCourses
instance ToJSON VCourses

vcourses :: Getter [Course] VCourses
vcourses = lens mapCourses const
    where mapCourses = VCourses . map mapCourse
          mapCourse course = VCourse (course ^. _courseId) (course ^. _name)
