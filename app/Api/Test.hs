{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Test where

import Prelude hiding (log)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import Data.ByteString.Lens
import Data.Text.Strict.Lens
import Data.Aeson.Lens
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.IO
import System.Exit
import System.FilePath
import System.Directory
import System.Process
import Servant
import Servant.API.WebSocket
import Network.WebSockets

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

                  --sendTextData conn $ res ^. re _JSON . packed

type Tester = ReaderT (TQueue String) (ExceptT TestError IO)

data TestError = TestUnknownError
               | TestUnsupportedLanguageError
               | TestCompileError
               | TestWrongAnswerError Test
               | TestRuntimeError Test

runTester :: TQueue String -> Tester a -> IO (Either TestError a)
runTester q m = runExceptT $ runReaderT m q

log :: String -> Tester ()
log x = ask >>= \q -> liftIO $ atomically $ writeTQueue q x

finally :: Tester a -> Tester b -> Tester b
finally mfin m = do
    x <- tryError m
    case x of
        Left e -> mfin >> throwError e
        Right a -> mfin >> pure a

unwrap :: Maybe a -> Tester a
unwrap = maybe (throwError TestUnknownError) pure

data Language = Haskell | C | Cpp | Python
    deriving Generic

instance FromJSON Language
instance ToJSON Language

options :: Language -> String -> (String, [String], [String], [String])
options lang filename = (source, compile lang, run lang, clean lang)
    where source = addExtension filename $ ext lang

          ext Haskell = "hs"
          ext C = "c"
          ext Cpp = "cpp"
          ext Python = "py"

          compile Haskell = ["ghc", "-o", filename, source]
          compile C = ["gcc", "-o", filename, source]
          compile Cpp = ["g++", "-o", filename, source]
          compile Python = []

          run Python = ["python3", source]
          run _ = ["./" ++ filename]

          clean Haskell = [filename, addExtension filename "o", addExtension filename "hi", source]
          clean C = [filename, source]
          clean Cpp = [filename, source]
          clean Python = [source]

cleanup :: [String] -> Tester ()
cleanup = mapM_ $ liftIO . tryError . removeFile

checkOutput :: String -> String -> Bool
checkOutput x y = x' == y'
    where (x', y') = (x, y) & both %~ stripLines . strip

          stripLines = over lined strip
          strip = over packed T.strip

test :: String -> String -> [Test] -> Tester ()
test input langJSON tests = do
    lang <- withError (const TestUnsupportedLanguageError) $ unwrap $ langJSON ^. pre _JSON

    filename <- liftIO getPOSIXTime >>= pure . ("tmp"</>) . (show :: Int -> FilePath) . round
    let (source, compileOptions, runOptions, cleanOptions) = options lang filename
    liftIO $ writeFile source input

    finally (cleanup cleanOptions) $ do
        case compileOptions of
            [] -> pure ()
            (prog:args) -> do
                let process = (proc prog args){ std_out = CreatePipe, std_err = CreatePipe }
                handle <- view _4 <$> liftIO (createProcess process)

                exitCode <- liftIO $ waitForProcess handle
                case exitCode of
                    ExitSuccess -> log "Compile ok"
                    ExitFailure code -> do
                        log $ "Compile error. Program finished with exit code " ++ show code ++ "."
                        throwError TestCompileError

        case runOptions of
            [] -> pure ()
            (prog:args) -> forM_ tests $ \t -> do
                let process = (proc prog args){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
                handles <- liftIO $ createProcess process

                let handle = handles ^. _4
                stdin <- unwrap $ handles ^. _1
                stdout <- unwrap $ handles ^. _2

                exitCode <- liftIO $ hPutStrLn stdin (t ^. _input) >> hClose stdin >> waitForProcess handle
                case exitCode of
                    ExitSuccess -> do
                        out <- liftIO $ hGetContents stdout
                        let correct = checkOutput (t ^. _output) out

                        unless correct $ do
                            log "Wrong answer"
                            throwError $ TestWrongAnswerError t

                        log "Ok"
                    ExitFailure code -> do
                        log $ "Runtime error. Program finished with exit code " ++ show code ++ "."
                        throwError $ TestRuntimeError t
