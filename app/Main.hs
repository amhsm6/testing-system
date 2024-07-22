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
import qualified Data.Map as M
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Environment
import Crypto.BCrypt
import Web.JWT
import Configuration.Dotenv
import Servant
import Servant.API.WebSocket
import Network.WebSockets
import Network.Wai.Handler.Warp
import Network.HTTP.Media ((//))

import Tester
import Database.Monad
import Database.Course
import Database.User

data HTML

instance Accept HTML where
    contentType _ =  ("text" ^. packedChars) // ("html" ^. packedChars)

instance MimeRender HTML String where
    mimeRender _ = view packedChars

type Api = "api" :> "courses" :> Get '[JSON] [Course]
      :<|> "api" :> "course" :> Capture "courseId" Int :> Get '[JSON] (Maybe Course)
      :<|> "api" :> "submit" :> Capture "problemId" Int :> WebSocket
      :<|> "api" :> "register" :> ReqBody '[JSON] User :> Post '[JSON] RegResp
      :<|> Get '[HTML] String
      :<|> "course" :> Capture "courseId" Int :> Get '[HTML] String
      :<|> Raw

data RegResp = EmailInUse
             | Ok { token :: String }
             deriving Generic

instance FromJSON RegResp
instance ToJSON RegResp

api :: Proxy Api
api = Proxy

server :: ServerT Api DB
server = coursesH :<|> courseH :<|> submitH :<|> regH :<|> indexPageH :<|> coursePageH :<|> staticH
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

          regH user = do
              userExists <- has _Just <$> getUser (user ^. _email)
              if userExists then do
                  pure EmailInUse
              else do
                  hashedUser <- forMOf (_pass . packedChars) user $ \pass -> do
                      hashed <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy pass
                      maybe (throwError err500) pure hashed

                  -- userId <- createUser hashedUser

                  let payload = [("email", hashedUser ^. _email), ("pass", hashedUser ^. _pass)]

                  secret <- liftIO $ getEnv "JWT_SECRET"
                  let payload' = payload & traverse . _1 %~ view packed
                                         & traverse . _2 %~ review _JSON
                      claims = mempty { unregisteredClaims = ClaimsMap $ M.fromList payload' }
                      token = encodeSigned (EncodeHMACSecret $ secret ^. packedChars) mempty claims

                  pure $ Ok $ token ^. unpacked

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
