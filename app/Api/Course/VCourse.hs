{-# LANGUAGE DeriveGeneric #-}

module Api.Course.VCourse
    ( VCourse, vcourse
    ) where

import Control.Monad
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import DB.Course
import DB.Problem

data VCourse = VCourse (Maybe VCourse_)
    deriving Generic

data VCourse_ = VCourse_ { name :: String
                         , problems :: [VProblem]
                         }
                         deriving Generic

data VProblem = VProblem { id :: Int
                         , description :: String
                         }
                         deriving Generic

instance FromJSON VProblem
instance ToJSON VProblem

instance FromJSON VCourse_
instance ToJSON VCourse_

instance FromJSON VCourse
instance ToJSON VCourse

vcourse :: Getter (Maybe Course) VCourse
vcourse = lens (VCourse . fmap mapCourse) const
    where mapCourse course = VCourse_ (course ^. _name) (course ^.. _problems . traverse . to mapProblem)
          mapProblem problem = VProblem (problem ^. _problemId) (problem ^. _description)
