module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.Text as T
import System.FilePath
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Tester

{-logger :: TVar TestLogs -> IO ()
logger logs = do
    processed <- atomically $ newTVar 0
    void $ forkIO $ forever $ do
        x <- atomically $ do
            x <- readTVar logs
            y <- readTVar processed
            check $ length x > y
            writeTVar processed $ length x
            pure x
        print x-}

type Reply = ReaderT Request IO Response

reply :: Request -> (Response -> IO ResponseReceived) -> Reply -> IO ResponseReceived
reply req resp reply = runReaderT reply req >>= resp

err404 :: Reply
err404 = pure $ responseLBS notFound404 [] B.empty

static :: FilePath -> Reply
static path = pure $ responseFile ok200 [] ("html" </> path) Nothing

getRoutes :: Reply
getRoutes = do
    req <- ask

    let path = map T.unpack $ pathInfo req 
    case path of
        [] -> static "index.html"
        ["courses"] -> static "courses.html"
        ["course"] -> static "course.html"
        ["problem"] -> static "problem.html"
        _ -> err404

submit :: Reply
submit = undefined

postRoutes :: Reply
postRoutes = do
    req <- ask

    let path = map T.unpack $ pathInfo req 
    case path of
        ["submit"] -> submit
        _ -> err404

main :: IO ()
main = do
    {-logs <- atomically $ newTVar []
    logger logs

    let foo = test (TestSimple C $ (map (\x -> (show x, show $ x * 2)) [1..10]) ++ [("1", "")]) $
                  "int main(void) { int x; scanf(\"%d\", &x); usleep(1000000); printf(\"%d\", x * 2); return 0; }"
    runTester logs foo >>= print-}
    putStrLn "Server is running on port 8080"
    run 8080 $ \req resp -> reply req resp $ do
        case requestMethod req of
            methodGet -> getRoutes
            methodPost -> postRoutes
            _ -> err404
