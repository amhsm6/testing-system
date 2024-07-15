{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Data.ByteString.Lens
import Data.Text.Lazy.Lens
import Configuration.Dotenv
import Servant
import Servant.API.WebSocket
import Network.WebSockets hiding (Headers)
import Network.Wai.Handler.Warp
import Network.HTTP.Media ((//))

import Database

data HTML

instance Accept HTML where
    contentType _ = Chars "text" // Chars "html"

instance MimeRender HTML String where
    mimeRender _ = Chars

type Api = "api" :> "courses" :> Get '[JSON] [Course]
      :<|> "api" :> "course" :> Capture "courseId" Int :> Get '[JSON] Course
      :<|> "api" :> "submit" :> Capture "problemId" Int :> WebSocket
      :<|> Get '[HTML] String
      :<|> "course" :> Capture "courseId" Int :> Get '[HTML] String
      :<|> Raw

api :: Proxy Api
api = Proxy

server :: ServerT Api DB
server = coursesH :<|> courseH :<|> submitH :<|> staticH
    where coursesH = getCourses

          courseH = getCourse

          submitH problemId conn = liftIO $ do
              src <- view unpacked <$> receiveData conn
              print problemId
              print src

              sendTextData conn $ view packed $ "foo " ++ show problemId

          staticH = liftIO (readFile "static/html/index.html") :<|>
                    const (liftIO $ readFile "static/html/course.html") :<|>
                    serveDirectoryWebApp "static"

main :: IO ()
main = do
    loadFile defaultConfig
    putStrLn "Server is running on port 8080"
    run 8080 $ serve api $ hoistServer api runDB server
