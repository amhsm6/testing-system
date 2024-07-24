{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Testing where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import Data.Aeson.Lens
import qualified Data.Text as T
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

type Action = ExceptT Response DB

runAction :: Action Response -> DB Response
runAction m = either id id <$> runExceptT m

submit :: Int -> IO (Maybe (String, Language)) -> (Response -> IO ()) -> Action Response
submit problemId recv resp = do
    problem <- lift $ getProblem problemId
    when (has _Nothing problem) $ do
        throwError ProblemDoesNotExist

    (src, lang) <- liftIO recv >>= maybe (throwError UnsupportedLanguage) pure
    tests <- lift $ getTests problemId

    liftIO $ do
        q <- atomically newTQueue
        tlogs <- atomically $ newTVar []

        let sendLogs = forever $ do
                logs <- atomically $ do
                    log <- readTQueue q
                    modifyTVar tlogs (++[log])

                    readTVar tlogs
                    
                resp $ ResponseLogs logs
                threadDelay 10
        logThread <- forkIO sendLogs

        res <- runTester q $ test src lang tests

        atomically $ isEmptyTQueue q >>= check
        killThread logThread

        case res of
            Right () -> pure ResponsePassed
            Left e -> pure $ TestingError e

testService :: ServerT TestApi DB
testService = submitH
    where submitH problemId conn = do
              let recv = preview (_JSON . _VInput) <$> (receiveData conn :: IO T.Text)
                  resp x = sendTextData conn (x ^. re (_JSON . _VResponse) :: T.Text)

              runAction (submit problemId recv resp) >>= liftIO . resp
