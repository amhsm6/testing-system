{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import Data.ByteString.Lens
import Data.Text.Strict.Lens
import Data.Aeson.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Crypto.BCrypt
import Configuration.Dotenv
import Servant
import Servant.API.WebSocket
import Network.WebSockets
import Network.Wai.Handler.Warp
import Network.HTTP.Media ((//))

import DB
import Api.Test
import Api.Course
import Api.User

data HTML

instance Accept HTML where
    contentType _ =  ("text" ^. packedChars) // ("html" ^. packedChars)

instance MimeRender HTML String where
    mimeRender _ = view packedChars

type Api = "api" :> "courses" :> Get '[JSON] [Course]
      :<|> "api" :> "course" :> Capture "courseId" Int :> Get '[JSON] (Maybe Course)
      :<|> "api" :> "submit" :> Capture "problemId" Int :> WebSocket
      :<|> "api" :> "register" :> ReqBody '[JSON] User :> Post '[JSON] RegResp
      :<|> "api" :> "auth" :> ReqBody '[JSON] User :> Post '[JSON] AuthResp
      :<|> Get '[HTML] String
      :<|> "course" :> Capture "courseId" Int :> Get '[HTML] String
      :<|> Raw

api :: Proxy Api
api = Proxy

server :: ServerT Api DB
server = coursesH :<|> courseH :<|> submitH :<|> regH :<|> authH :<|> indexPageH :<|> coursePageH :<|> staticH
    where coursesH = getCourses

          courseH = getCourse

          submitH problemId conn = do
              validProblem <- checkProblem problemId
              unless validProblem $ do
                  liftIO $ sendClose conn $ "The problem does not exist" ^. packed
                  throwError err404

              src <- view unpacked <$> liftIO (receiveData conn)
              lang <- view unpacked <$> liftIO (receiveData conn)
              tests <- getTests problemId

              liftIO $ do
                  q <- atomically newTQueue
                  tlogs <- atomically $ newTVar []

                  let sendLogs = forever $ do
                          logs <- atomically $ do
                              log <- readTQueue q
                              modifyTVar tlogs (++[log])

                              readTVar tlogs
                              
                          sendTextData conn $ logs ^. re _JSON . packed
                          threadDelay 10
                  logThread <- forkIO sendLogs

                  res <- runTester q $ test src lang tests

                  atomically $ isEmptyTQueue q >>= check
                  killThread logThread

                  sendTextData conn $ res ^. re _JSON . packed

          regH userIn = do
              userExists <- has _Just <$> getUser (userIn ^. _email)
              if userExists then do
                  pure RegEmailInUse
              else do
                  user <- forMOf (_pass . packedChars) userIn $ \pass -> do
                      hashed <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy pass
                      maybe (throwError err500) pure hashed

                  -- userId <- createUser user
                  let userId = 1509

                  liftIO (generateToken userId) >>= pure . RegOk

          authH userIn = do
              res <- getUser $ userIn ^. _email
              let validUser = do
                      user <- res
                      guard $ validatePassword (user ^. _pass . packedChars) (userIn ^. _pass . packedChars)
                      Just user

              case validUser of
                  Just user -> pure $ AuthOk $ undefined --generateToken $ user ^?! _userId
                  Nothing -> pure AuthWrongCredentials

          indexPageH = liftIO $ readFile "static/html/index.html"

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
