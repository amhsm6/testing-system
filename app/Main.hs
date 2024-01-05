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

type Api = "api" :> "courses" :> Get '[JSON] Courses
      :<|> "api" :> "course" :> Capture "courseId" Int :> Get '[JSON] Course
      :<|> "api" :> "problem" :> Capture "problemId" Int :> Get '[JSON] Problem
      :<|> "api" :> "submit" :> ReqBody '[PlainText] String :> WebSocket
      :<|> Get '[PlainText] (Headers '[Header "Content-Type" String] String)
      :<|> "courses" :> Get '[PlainText] (Headers '[Header "Content-Type" String] String)
      :<|> "course" :> Get '[PlainText] (Headers '[Header "Content-Type" String] String)
      :<|> "problem" :> Get '[PlainText] (Headers '[Header "Content-Type" String] String)
      :<|> Raw

api :: Proxy Api
api = Proxy

server :: ServerT Api DB
server = coursesH :<|> courseH :<|> problemH :<|> submitH :<|> static
    where coursesH = pure [Course 0]

          courseH courseId = liftIO (print courseId) >> pure (Course courseId)

          problemH = undefined

          submitH _ conn = liftIO $ sendTextData conn $ T.pack "foo"

          static = liftIO (addHeader "text/html" <$> readFile "static/html/index.html") :<|>
                   liftIO (addHeader "text/html" <$> readFile "static/html/courses.html") :<|>
                   liftIO (addHeader "text/html" <$> readFile "static/html/course.html") :<|>
                   liftIO (addHeader "text/html" <$> readFile "static/html/problem.html") :<|>
                   serveDirectoryWebApp "static"

main :: IO ()
main = do
    putStrLn "Server is running on port 8080"
    run 8080 $ serve api $ hoistServer api runDB server
