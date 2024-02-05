{-# LANGUAGE TypeApplications #-}
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

data TestError = TestGenericFailed
               | TestSimpleCompileError
               | TestSimpleFailed
               | TestCustomFailed

type TestLogs = [String]

runTester :: TVar TestLogs -> Tester a -> IO (Either TestError a)
runTester logs m = runReaderT (runExceptT m) logs

log :: String -> Tester ()
log x = do
    logs <- ask
    liftIO $ atomically $ modifyTVar logs (++[x])

finally :: Tester () -> Tester () -> Tester ()
finally mfin m = (m >> mfin) `catchError` (const mfin)

data Test = TestGeneric String
          | TestSimple Language [(String, String)]
          | TestCustom String String
          deriving Generic

data Language = Haskell | C | Cpp | Python
    deriving Generic

instance FromJSON Language
instance ToJSON Language

instance FromJSON Test
instance ToJSON Test

test :: Test -> String -> Tester ()
test (TestGeneric output) input = unless (input == output) $ throwError TestGenericFailed
test (TestSimple lang subtests) input = execProgram lang input subtests 
test (TestCustom cmd output) input = runCustomTest cmd input output

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
cleanup = mapM_ $ \x -> liftIO (removeFile x) `catchError` (const $ pure ())

execProgram :: Language -> String -> [(String, String)] -> Tester ()
execProgram lang input subtests = do
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
                        throwError TestSimpleCompileError

        case runOptions of
            [] -> pure ()
            (prog:args) -> forM_ subtests $ \(x, y) -> do
                let process = (proc prog args){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
                (mbStdin, mbStdout, mbStderr, handle) <- liftIO $ createProcess process
                let stdin = maybe undefined id mbStdin
                    stdout = maybe undefined id mbStdout
                    stderr = maybe undefined id mbStderr

                exitCode <- liftIO $ hPutStrLn stdin x >> hFlush stdin >> waitForProcess handle
                case exitCode of
                    ExitSuccess -> do
                        y' <- liftIO $ hGetContents stdout
                        unless (y == y') $ log "Wrong answer" >> throwError TestSimpleFailed
                        log "Ok"
                    ExitFailure code -> do
                        let msg = "Runtime error. Program finished with exit code " ++ show code
                        liftIO (hGetContents stderr) >>= log . (++msg)
                        throwError TestSimpleFailed

runCustomTest :: String -> String -> String -> Tester ()
runCustomTest cmd input output = do
    let process = (proc cmd []){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    (mbStdin, mbStdout, mbStderr, handle) <- liftIO $ createProcess process
    let stdin = maybe undefined id mbStdin
        stdout = maybe undefined id mbStdout
        stderr = maybe undefined id mbStderr

    exitCode <- liftIO $ hPutStrLn stdin input >> hFlush stdin >> waitForProcess handle
    case exitCode of
        ExitSuccess -> do
            output' <- liftIO $ hGetContents stdout
            unless (output == output') $ log "Wrong answer" >> throwError TestCustomFailed
            log "Ok"
        ExitFailure code -> do
            let msg = "Runtime error. Program finished with exit code " ++ show code
            liftIO (hGetContents stderr) >>= log . (++msg)
            throwError TestCustomFailed
