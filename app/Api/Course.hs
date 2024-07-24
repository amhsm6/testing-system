{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Course where

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
    where coursesH = review _VCourses <$> getCourses

          courseH courseId = review _VCourse <$> getCourse courseId
