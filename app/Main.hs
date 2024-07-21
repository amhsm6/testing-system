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
import Network.WebSockets hiding (Text)
import Network.Wai.Handler.Warp
import Network.HTTP.Media ((//))

import Tester
import Database.Monad
import Database.Course
import Database.User

data HTML

instance Accept HTML where
    contentType _ = Chars "text" // Chars "html"

instance MimeRender HTML String where
    mimeRender _ = view packedChars

type Api = "api" :> "courses" :> Get '[JSON] [Course]
      :<|> "api" :> "course" :> Capture "courseId" Int :> Get '[JSON] (Maybe Course)
      :<|> "api" :> "submit" :> Capture "problemId" Int :> WebSocket
      :<|> "api" :> "register" :> ReqBody '[JSON] User :> Post '[JSON] String
      :<|> Get '[HTML] String
      :<|> "course" :> Capture "courseId" Int :> Get '[HTML] String
      :<|> Raw

api :: Proxy Api
api = Proxy

server :: ServerT Api DB
server = coursesH :<|> courseH :<|> submitH :<|> regH :<|> indexPageH :<|> coursePageH :<|> staticH
    where coursesH = getCourses

          courseH = getCourse

          submitH problemId conn = do
              validProblem <- checkProblem problemId
              unless validProblem $ do
                  liftIO $ sendClose conn $ Text "The problem does not exist"
                  throwError err404

              src <- view unpacked <$> liftIO (receiveData conn)
              lang <- view unpacked <$> liftIO (receiveData conn)
              tests <- getTests problemId

              liftIO $ do
                  logs <- atomically $ newTVar []
                  logsRead <- atomically $ newTVar 0

                  let sendLogs = forever $ do
                          xs <- atomically $ do
                              xs <- readTVar logs
                              y <- readTVar logsRead
                              check $ length xs > y

                              writeTVar logsRead $ length xs
                              pure xs
                              
                          sendTextData conn $ xs ^. re _JSON . packed
                  thread <- forkIO sendLogs

                  res <- runTester logs $ test src lang tests

                  atomically $ do
                      xs <- readTVar logs
                      y <- readTVar logsRead
                      check $ length xs == y
                  killThread thread

                  sendTextData conn $ res ^. re _JSON . packed

          regH user = undefined

          indexPageH = liftIO $ readFile "static/html/index.html"

          coursePageH courseId = do
              course <- getCourse courseId
              case course of
                  Just _ -> liftIO $ readFile "static/html/course.html"
                  Nothing -> throwError $ err404 { errBody = Chars "The course does not exist" }

          staticH = serveDirectoryWebApp "static"

main :: IO ()
main = do
    loadFile defaultConfig
    putStrLn "Server is running on port 8080"
    run 8080 $ serve api $ hoistServer api runDB server
