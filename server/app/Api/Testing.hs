{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Testing where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Text as T
import Data.Aeson.Lens
import Servant
import Servant.API.WebSocket
import Network.WebSockets hiding (Response)

import Api.Testing.Core
import Api.Testing.Response
import Api.Testing.VInput
import Api.Testing.VResponse
import DB
import DB.Problem
import DB.Test

type TestApi = "api" :> "submit" :> Capture "problemId" Int :> WebSocket

testService :: ServerT TestApi DB
testService = submitH
    where submitH problemId conn = do
              let recv = receiveData conn :: IO T.Text
                  resp x = sendTextData conn (x ^. re (_JSON . _VResponse) :: T.Text)

              flip runContT (liftIO . resp) $ do
                  callCC $ \exit -> do
                      problem <- lift $ getProblem problemId
                      when (has _Nothing problem) $ do
                          void $ exit ProblemDoesNotExist

                      req <- liftIO recv
                      json <- maybe (lift $ throwError err400) pure $ req ^. pre _JSON
                      (src, lang) <- maybe (exit UnsupportedLanguage) pure $ json ^. pre _VInput

                      tests <- lift $ getTests problemId

                      liftIO $ do
                          q     <- atomically newTQueue
                          tlogs <- atomically $ newTVar []
                          tidle <- atomically $ newTVar True

                          sendLogs <- forkIO $ do
                              forever $ do
                                  atomically $ do
                                      log <- readTQueue q
                                      modifyTVar tlogs (++[log])

                                  atomically $ writeTVar tidle False
                                  atomically (readTVar tlogs) >>= resp . ResponseLogs
                                  atomically $ writeTVar tidle True

                          res <- runTester q $ test src lang tests

                          atomically $ do
                              isEmptyTQueue q >>= check
                              readTVar tidle >>= check
                          killThread sendLogs

                          case res of
                              Right () -> pure ResponsePassed
                              Left e   -> pure $ TestingError e
