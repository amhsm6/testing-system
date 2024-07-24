{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Testing where

import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import Data.Text.Lazy.Lens
import Data.Aeson.Lens
import Servant
import Servant.API.WebSocket
import Network.WebSockets

import Api.Testing.Core
import Api.Testing.VInput
import DB
import DB.Problem
import DB.Test

type TestApi = "api" :> "submit" :> Capture "problemId" Int :> WebSocket

testService :: ServerT TestApi DB
testService = submitH
    where submitH problemId conn = do
              problem <- getProblem problemId
              when (has _Nothing problem) $ do
                  liftIO $ sendClose conn $ "The problem does not exist" ^. packed
                  throwError err404

              src <- view unpacked <$> liftIO (receiveData conn)
              lang <- undefined --view unpacked <$> liftIO (receiveData conn)
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

                  --sendTextData conn $ res ^. re _JSON . packed
