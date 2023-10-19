{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE UndecidableInstances #-}

module Tester where

import Prelude hiding (log)
import Control.Monad
import Control.Monad.Writer
import Control.Exception
import Data.Time.Clock.POSIX
import System.IO
import System.Process
import System.Exit hiding (die)
import System.Directory
import System.FilePath

newtype EitherT m a b = EitherT (m (Either a b))

runEitherT :: EitherT m a b -> m (Either a b)
runEitherT (EitherT m) = m

instance Monad m => Monad (EitherT m a) where
    return = pure
    (EitherT m) >>= g = EitherT $ m >>= either (pure . Left) (runEitherT . g)

instance Monad m => Applicative (EitherT m a) where
    pure = EitherT . pure . Right
    m1 <*> m2 = m1 >>= \f -> m2 >>= pure . f

instance Monad m => Functor (EitherT m a) where
    fmap f m = m >>= pure . f

instance MonadIO m => MonadIO (EitherT m a) where
    liftIO io = EitherT $ liftIO io >>= pure . Right

instance MonadWriter w m => MonadWriter w (EitherT m a) where
    tell w = EitherT $ tell w >>= pure . Right
    listen (EitherT m) = EitherT $ listen m >>= \(e, w) -> pure $ e >>= Right . (,w)
    pass (EitherT m) = EitherT $ m >>= either (pure . Left) (pass . pure >=> pure . Right)

data Test = TestGeneric String
          | TestSimple Language [(String, String)]
          | TestCustom String String

data Language = Haskell | C | Cpp | Python

data TestError = TestNotFound
               | TestGenericFailed
               | TestSimpleCompileError
               | TestSimpleFailed
               | TestCustomFailed
               deriving Show

type TestLogs = [String]

type Tester = EitherT (WriterT TestLogs IO) TestError

runTester :: Tester a -> IO (Either TestError a, TestLogs)
runTester = runWriterT . runEitherT

die :: TestError -> Tester a
die err = EitherT $ WriterT $ pure (Left err, [])

finallyTester :: Tester a -> Tester b -> Tester b
finallyTester mFinally m = do
    (e, logs) <- liftIO $ runTester m
    tell logs
    mFinally
    case e of
        Left err -> die err
        Right x -> pure x

log :: String -> Tester ()
log x = tell [x]

test :: Test -> String -> Tester ()
test (TestGeneric output) input = unless (input == output) $ die TestGenericFailed
test (TestSimple lang subtests) input = execProgram lang input subtests 
test (TestCustom cmd output) input = runCustomTest cmd input output

execProgram :: Language -> String -> [(String, String)] -> Tester ()
execProgram lang input subtests = do
    filename <- liftIO getPOSIXTime >>= pure . ("tmp"</>) . (show :: Int -> FilePath) . round
    let (source, compile, run, clean) = options lang filename
    liftIO $ writeFile source input

    finallyTester (cleanup clean) $ do
        case compile of
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
                        die TestSimpleCompileError

        case run of
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
                        unless (y == y') $ log "Wrong answer" >> die TestSimpleFailed
                        log "Ok"
                    ExitFailure code -> do
                        let msg = "Runtime error. Program finished with exit code " ++ show code
                        liftIO (hGetContents stderr) >>= log . (++msg)
                        die TestSimpleFailed

cleanup :: [String] -> Tester ()
cleanup = liftIO . mapM_ (void . try @IOException . removeFile)

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

runCustomTest :: String -> String -> String -> Tester ()
runCustomTest cmd input output = undefined
