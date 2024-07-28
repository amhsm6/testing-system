{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Static where

import Control.Monad.Trans
import Control.Lens
import Data.ByteString.Lens
import Servant
import Network.HTTP.Media ((//))

import DB
import DB.Course

data HTML

instance Accept HTML where
    contentType _ =  ("text" ^. packedChars) // ("html" ^. packedChars)

instance MimeRender HTML String where
    mimeRender _ = view packedChars

type StaticApi = Get '[HTML] String
            :<|> "course" :> Capture "courseId" Int :> Get '[HTML] String
            :<|> Raw

staticService :: ServerT StaticApi DB
staticService = indexPageH :<|> coursePageH :<|> staticH
    where indexPageH = liftIO $ readFile "static/html/index.html"

          coursePageH courseId = do
              course <- getCourse courseId
              case course of
                  Just _ -> liftIO $ readFile "static/html/course.html"
                  Nothing -> throwError $ err404 { errBody = "The course does not exist" ^. packedChars }

          staticH = serveDirectoryWebApp "static"
