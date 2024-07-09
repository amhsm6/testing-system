{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
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
      :<|> "courses" :> Get '[PlainText] (Headers '[Header "Content-Type" String] String)
      :<|> "course" :> Get '[PlainText] (Headers '[Header "Content-Type" String] String)
      :<|> "problem" :> Get '[PlainText] (Headers '[Header "Content-Type" String] String)
      :<|> Raw

api :: Proxy Api
api = Proxy

server :: ServerT Api DB
server = coursesH :<|> problemsH :<|> submitH :<|> static
    where coursesH = getCourses

          problemsH courseId = undefined

          submitH problemId conn = liftIO $ do
              src <- T.unpack <$> receiveData conn
              print problemId
              print src

              sendTextData conn $ T.pack $ "foo " ++ show problemId

          static = liftIO (addHeader "text/html" <$> readFile "static/html/index.html") :<|>
                   liftIO (addHeader "text/html" <$> readFile "static/html/courses.html") :<|>
                   liftIO (addHeader "text/html" <$> readFile "static/html/course.html") :<|>
                   liftIO (addHeader "text/html" <$> readFile "static/html/problem.html") :<|>
                   serveDirectoryWebApp "static"

main :: IO ()
main = do
    putStrLn "Server is running on port 8080"
    run 8080 $ serve api $ hoistServer api runDB server
