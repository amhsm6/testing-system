module Tester where

import Prelude hiding (log)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import System.IO
import System.Exit
import System.FilePath
import System.Directory
import System.Process

type Tester = ReaderT (TVar TestLogs) (ExceptT TestError IO)

data TestError = TestUnknownError
               | TestCompileError
               | TestFailed

type TestLogs = [String]

runTester :: TVar TestLogs -> Tester a -> IO (Either TestError a)
runTester logs m = runExceptT $ runReaderT m logs

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

data Language = Haskell | C | Cpp | Python

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
                stdout <- maybe (throwError TestUnknownError) pure mbStdout
                stderr <- maybe (throwError TestUnknownError) pure mbStderr

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
                stdin <- maybe (throwError TestUnknownError) pure mbStdin
                stdout <- maybe (throwError TestUnknownError) pure mbStdout
                stderr <- maybe (throwError TestUnknownError) pure mbStderr

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
