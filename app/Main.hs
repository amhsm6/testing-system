{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import Configuration.Dotenv
import Servant
import Servant.API.WebSocket
import Network.WebSockets hiding (Headers)
import Network.Wai.Handler.Warp

import Tester
import Database

type Api = "api" :> "courses" :> Get '[JSON] [Course]
      :<|> "api" :> "problems" :> Capture "courseId" Int :> Get '[JSON] [Problem]
      :<|> "api" :> "submit" :> Capture "problemId" Int :> WebSocket
      :<|> Get '[PlainText] (Headers '[Header "Content-Type" String] String)
      :<|> "course" :> Capture "courseId" Int :> Get '[PlainText] (Headers '[Header "Content-Type" String] String)
      :<|> "problem" :> Capture "problemId" Int :> Get '[PlainText] (Headers '[Header "Content-Type" String] String)
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

          staticH = liftIO (addHeader "text/html" <$> readFile "static/html/index.html") :<|>
                    const (liftIO (addHeader "text/html" <$> readFile "static/html/course.html")) :<|>
                    const (liftIO (addHeader "text/html" <$> readFile "static/html/problem.html")) :<|>
                    serveDirectoryWebApp "static"

main :: IO ()
main = do
    loadFile defaultConfig
    putStrLn "Server is running on port 8080"
    run 8080 $ serve api $ hoistServer api runDB server
