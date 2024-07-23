{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Course where

import Control.Monad
import Control.Lens
import Servant

import Api.Course.CoursesResp
import Api.Course.CourseResp
import DB
import DB.Course

type CourseApi = "api" :> "courses" :> Get '[JSON] CoursesResp
            :<|> "api" :> "course" :> Capture "courseId" Int :> Get '[JSON] (Maybe CourseResp)

courseService :: ServerT CourseApi DB
courseService = coursesH :<|> courseH
    where coursesH = view coursesResp <$> getCourses

          courseH courseId = view courseResp <$> getCourse courseId
