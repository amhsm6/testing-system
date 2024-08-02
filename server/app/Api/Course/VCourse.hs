{-# LANGUAGE DeriveGeneric #-}

module Api.Course.VCourse
    ( VCourse, _VCourse
    ) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import DB.Course
import DB.Problem

data VCourse = VCourse (Maybe VCourse')
    deriving Generic

data VCourse' = VCourse' { name :: String
                         , problems :: [VProblem]
                         }
                         deriving Generic

data VProblem = VProblem { id :: Int
                         , description :: String
                         }
                         deriving Generic

instance FromJSON VProblem
instance ToJSON VProblem

instance FromJSON VCourse'
instance ToJSON VCourse'

instance FromJSON VCourse
instance ToJSON VCourse

_VCourse :: Prism' VCourse (Maybe Course)
_VCourse = prism (VCourse . fmap mapCourse) Left
    where mapCourse course = VCourse' (course ^. _name) (course ^.. _problems . traverse . to mapProblem)
          mapProblem problem = VProblem (problem ^. _problemId) (problem ^. _description)
