{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import Data.ByteString.Lens
import Data.Text.Lazy.Lens
import Data.Aeson.Lens
import Configuration.Dotenv
import Servant
import Servant.API.WebSocket
import Network.WebSockets
import Network.Wai.Handler.Warp
import Network.HTTP.Media ((//))

import Tester
import Database

data HTML

instance Accept HTML where
    contentType _ = Chars "text" // Chars "html"

instance MimeRender HTML String where
    mimeRender _ = view packedChars

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

          submitH problemId conn = do
              src <- view unpacked <$> liftIO (receiveData conn)
              tests <- getTests problemId

              logs <- liftIO $ atomically $ newTVar []
              logsRead <- liftIO $ atomically $ newTVar 0

              let sendLogs = forever $ do
                      xs <- atomically $ do
                          xs <- readTVar logs
                          y <- readTVar logsRead
                          check $ length xs > y

                          writeTVar logsRead $ length xs
                          pure xs

                      sendTextData conn $ xs ^. re _JSON . packed
              thread <- liftIO $ forkIO sendLogs

              let t = Test Haskell tests
              res <- liftIO $ runTester logs $ test t src

              liftIO $ killThread thread

              liftIO $ print res

          staticH = liftIO (readFile "static/html/index.html") :<|>
                    const (liftIO $ readFile "static/html/course.html") :<|>
                    serveDirectoryWebApp "static"

main :: IO ()
main = do
    loadFile defaultConfig
    putStrLn "Server is running on port 8080"
    run 8080 $ serve api $ hoistServer api runDB server
