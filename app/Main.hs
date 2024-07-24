{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Data.ByteString.Lens
import Configuration.Dotenv
import Servant
import Network.Wai.Handler.Warp
import Network.HTTP.Media ((//))

import Api.Testing
import Api.Course
import DB
import DB.Course

data HTML

instance Accept HTML where
    contentType _ =  ("text" ^. packedChars) // ("html" ^. packedChars)

instance MimeRender HTML String where
    mimeRender _ = view packedChars

type Api = TestApi
      :<|> CourseApi
      :<|> Get '[HTML] String
      :<|> "course" :> Capture "courseId" Int :> Get '[HTML] String
      :<|> Raw

api :: Proxy Api
api = Proxy

server :: ServerT Api DB
server = testService :<|> courseService :<|> indexPageH :<|> coursePageH :<|> staticH
    where indexPageH = liftIO $ readFile "static/html/index.html"

          coursePageH courseId = do
              course <- getCourse courseId
              case course of
                  Just _ -> liftIO $ readFile "static/html/course.html"
                  Nothing -> throwError $ err404 { errBody = "The course does not exist" ^. packedChars }

          staticH = serveDirectoryWebApp "static"

main :: IO ()
main = do
    loadFile defaultConfig
    putStrLn "Server is running on port 8080"
    run 8080 $ serve api $ hoistServer api runDB server
