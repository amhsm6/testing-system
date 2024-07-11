{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.Text as T
import Configuration.Dotenv
import Servant
import Servant.API.WebSocket
import Network.WebSockets hiding (Headers)
import Network.Wai.Handler.Warp
import Network.HTTP.Media ((//))

import Tester
import Database

data HTML

instance Accept HTML where
    contentType _ = "text" // "html"

instance MimeRender HTML String where
    mimeRender _ = B.fromString

type Api = "api" :> "courses" :> Get '[JSON] [Course]
      :<|> "api" :> "problems" :> Capture "courseId" Int :> Get '[JSON] [Problem]
      :<|> "api" :> "submit" :> Capture "problemId" Int :> WebSocket
      :<|> Get '[HTML] String
      :<|> "course" :> Capture "courseId" Int :> Get '[HTML] String
      :<|> "problem" :> Capture "problemId" Int :> Get '[HTML] String
      :<|> Raw

api :: Proxy Api
api = Proxy

server :: ServerT Api DB
server = coursesH :<|> problemsH :<|> submitH :<|> staticH
    where coursesH = getCourses

          problemsH courseId = undefined

          submitH problemId conn = liftIO $ do
              src <- T.unpack <$> receiveData conn
              print problemId
              print src

              sendTextData conn $ T.pack $ "foo " ++ show problemId

          staticH = liftIO (readFile "static/html/index.html") :<|>
                    const (liftIO $ readFile "static/html/course.html") :<|>
                    const (liftIO $ readFile "static/html/problem.html") :<|>
                    serveDirectoryWebApp "static"

main :: IO ()
main = do
    loadFile defaultConfig
    putStrLn "Server is running on port 8080"
    run 8080 $ serve api $ hoistServer api runDB server
