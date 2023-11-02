module Main where

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Tester

logger :: TVar TestLogs -> IO ()
logger logs = do
    processed <- atomically $ newTVar 0
    void $ forkIO $ forever $ do
        x <- atomically $ do
            x <- readTVar logs
            y <- readTVar processed
            check $ length x > y
            writeTVar processed $ length x
            pure x
        print x

main :: IO ()
main = do
    logs <- atomically $ newTVar []
    logger logs

    let foo = test (TestSimple C $ (map (\x -> (show x, show $ x * 2)) [1..10]) ++ [("1", "")]) $
                  "int main(void) { int x; scanf(\"%d\", &x); usleep(1000000); printf(\"%d\", x * 2); return 0; }"
    runTester logs foo >>= print
