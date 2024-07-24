{-# LANGUAGE DeriveGeneric #-}

module Api.Course.VCourses
    ( VCourses, _VCourses
    ) where

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

_VCourses :: Prism' VCourses [Course]
_VCourses = prism mapCourses Left
    where mapCourses = VCourses . map mapCourse
          mapCourse course = VCourse (course ^. _courseId) (course ^. _name)
