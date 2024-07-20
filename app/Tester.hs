{-# LANGUAGE DeriveGeneric #-}

module Tester where

import Prelude hiding (log)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens hiding ((<.>))
import Control.Concurrent.STM
import Data.Text.Lens
import Data.Aeson.Lens
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Aeson
import GHC.Generics
import System.IO
import System.Exit
import System.FilePath
import System.Directory
import System.Process

import Database

type Tester = ReaderT (TVar TestLogs) (ExceptT TestError IO)

data TestError = TestUnknownError
               | TestUnsupportedLanguageError
               | TestCompileError
               | TestWrongAnswerError Test
               | TestRuntimeError Test
               deriving Generic

instance FromJSON TestError
instance ToJSON TestError

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

unwrap :: Maybe a -> Tester a
unwrap = maybe (throwError TestUnknownError) pure

data Language = Haskell | C | Cpp | Python
    deriving Generic

instance FromJSON Language
instance ToJSON Language

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
cleanup = mapM_ $ liftIO . tryError . removeFile

checkOutput :: String -> String -> Bool
checkOutput x y = x' == y'
    where (x', y') = (x, y) & both %~ stripLines . strip

          stripLines = over lined strip
          strip = over packed T.strip

test :: String -> String -> [Test] -> Tester ()
test input langJSON tests = do
    lang <- unwrap $ langJSON ^? _JSON

    filename <- liftIO getPOSIXTime >>= pure . ("tmp"</>) . (show :: Int -> FilePath) . round
    let (source, compileOptions, runOptions, cleanOptions) = options lang filename
    liftIO $ writeFile source input

    finally (cleanup cleanOptions) $ do
        case compileOptions of
            [] -> pure ()
            (prog:args) -> do
                let process = (proc prog args){ std_out = CreatePipe, std_err = CreatePipe }
                (_, mbStdout, mbStderr, handle) <- liftIO $ createProcess process
                stdout <- unwrap mbStdout
                stderr <- unwrap mbStderr

                exitCode <- liftIO $ waitForProcess handle
                case exitCode of
                    ExitSuccess -> liftIO (hGetContents stdout) >>= log . (++"Compile ok.")
                    ExitFailure code -> do
                        let msg = "Compile error. Program finished with exit code " ++ show code ++ "."
                        liftIO (hGetContents stderr) >>= log . (++msg)
                        throwError TestCompileError

        case runOptions of
            [] -> pure ()
            (prog:args) -> forM_ tests $ \t -> do
                let process = (proc prog args){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
                (mbStdin, mbStdout, mbStderr, handle) <- liftIO $ createProcess process
                stdin <- unwrap mbStdin
                stdout <- unwrap mbStdout
                stderr <- unwrap mbStderr

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
                        let msg = "Runtime error. Program finished with exit code " ++ show code ++ "."
                        liftIO (hGetContents stderr) >>= log . (++msg)
                        throwError $ TestRuntimeError t
