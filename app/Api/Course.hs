{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Course where

import Control.Monad
import Control.Lens
import Servant

import Api.Course.VCourses
import Api.Course.VCourse
import DB
import DB.Course

type CourseApi = "api" :> "courses" :> Get '[JSON] VCourses
            :<|> "api" :> "course" :> Capture "courseId" Int :> Get '[JSON] VCourse

courseService :: ServerT CourseApi DB
courseService = coursesH :<|> courseH
    where coursesH = view vcourses <$> getCourses

          courseH courseId = view vcourse <$> getCourse courseId
