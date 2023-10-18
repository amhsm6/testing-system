module Tester where

import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import System.Process
import System.Directory
import System.FilePath

newtype EitherT m a b = EitherT (m (Either a b))

instance Monad m => Monad (EitherT m a) where
    return = pure
    (EitherT m) >>= g = EitherT $ m >>= either (pure . Left) (unpack . g)
        where unpack (EitherT x) = x

instance Monad m => Applicative (EitherT m a) where
    pure = EitherT . pure . Right
    m1 <*> m2 = m1 >>= \f -> m2 >>= pure . f

instance Monad m => Functor (EitherT m a) where
    fmap f m = m >>= pure . f

instance MonadIO m => MonadIO (EitherT m a) where
    liftIO io = EitherT $ liftIO io >>= pure . Right

data Test = TestGeneric String
          | TestSimple Language [(String, String)]
          | TestCustom String String

data Language = Haskell | C | Cpp | Python

data TestError = TestNotFound
               | TestGenericFailed
               | TestSimpleFailed [String]
               | TestSimpleCompileError String
               | TestCustomFailed String

type Tester = EitherT IO TestError

die :: TestError -> Tester a
die = EitherT . pure . Left

test :: Test -> String -> Tester ()
test (TestGeneric output) input = unless (input == output) $ die TestGenericFailed
test (TestSimple lang subtests) input = execProgram lang input subtests 
test (TestCustom cmd output) input = runCustomTest cmd input output

execProgram :: Language -> String -> [(String, String)] -> Tester ()
execProgram lang input subtests = do
    filename <- ("tmp"</>) . (show :: Int -> FilePath) . round <$> liftIO getPOSIXTime
    let (source, compile, run, clean) = options lang filename
    liftIO $ writeFile source input

    case compile of
        [] -> pure ()
        (prog:args) -> do
            liftIO $ createProcess $ proc prog args
            pure ()

    case run of
        [] -> pure ()
        (prog:args) -> do
            liftIO $ createProcess $ proc prog args
            pure ()

    case clean of
        [] -> pure ()
        files -> liftIO $ mapM_ removeFile files

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
