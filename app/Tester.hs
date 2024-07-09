{-# LANGUAGE DeriveGeneric #-}

module Tester where

import Prelude hiding (log)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent.STM
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Aeson
import GHC.Generics
import System.IO
import System.Exit
import System.FilePath
import System.Directory
import System.Process

type Tester = ExceptT TestError (ReaderT (TVar TestLogs) IO)

data TestError = TestCompileError
               | TestFailed

type TestLogs = [String]

runTester :: TVar TestLogs -> Tester a -> IO (Either TestError a)
runTester logs m = runReaderT (runExceptT m) logs

log :: String -> Tester ()
log x = do
    logs <- ask
    liftIO $ atomically $ modifyTVar logs (++[x])

finally :: Tester a -> Tester b -> Tester b
finally mfin m = do
    x <- tryError m
    case x of
        Left e -> mfin >> throwError e
        Right x -> mfin >> pure x

data Test = Test Language [(String, String)]
    deriving Generic

data Language = Haskell | C | Cpp | Python
    deriving Generic

instance FromJSON Language
instance ToJSON Language

instance FromJSON Test
instance ToJSON Test

options :: Language -> String -> (String, [String], [String], [String])
options lang filename = (source, compile lang, run lang, clean lang)
    where source = filename <.> ext lang

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

          clean Haskell = [filename, filename <.> "o", filename <.> "hi", source]
          clean C = [filename, source]
          clean Cpp = [filename, source]
          clean Python = [source]

cleanup :: [String] -> Tester ()
cleanup = mapM_ $ \x -> liftIO $ tryError $ removeFile x

test :: Test -> String -> Tester ()
test (Test lang tests) input = do
    filename <- liftIO getPOSIXTime >>= pure . ("tmp"</>) . (show :: Int -> FilePath) . round
    let (source, compileOptions, runOptions, cleanOptions) = options lang filename
    liftIO $ writeFile source input

    finally (cleanup cleanOptions) $ do
        case compileOptions of
            [] -> pure ()
            (prog:args) -> do
                let process = (proc prog args){ std_out = CreatePipe, std_err = CreatePipe }
                (_, mbStdout, mbStderr, handle) <- liftIO $ createProcess process
                let stdout = maybe undefined id mbStdout
                    stderr = maybe undefined id mbStderr

                exitCode <- liftIO $ waitForProcess handle
                case exitCode of
                    ExitSuccess -> liftIO (hGetContents stdout) >>= log . (++"Compile ok")
                    ExitFailure x -> do
                        let msg = "Compile error. Program finished with exit code " ++ show x
                        liftIO (hGetContents stderr) >>= log . (++msg)
                        throwError TestCompileError

        case runOptions of
            [] -> pure ()
            (prog:args) -> forM_ tests $ \(x, y) -> do
                let process = (proc prog args){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
                (mbStdin, mbStdout, mbStderr, handle) <- liftIO $ createProcess process
                let stdin = maybe undefined id mbStdin
                    stdout = maybe undefined id mbStdout
                    stderr = maybe undefined id mbStderr

                exitCode <- liftIO $ hPutStrLn stdin x >> hFlush stdin >> waitForProcess handle
                case exitCode of
                    ExitSuccess -> do
                        y' <- liftIO $ hGetContents stdout
                        unless (y == y') $ log "Wrong answer" >> throwError TestFailed
                        log "Ok"
                    ExitFailure code -> do
                        let msg = "Runtime error. Program finished with exit code " ++ show code
                        liftIO (hGetContents stderr) >>= log . (++msg)
                        throwError TestFailed
