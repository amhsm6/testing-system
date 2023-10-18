module Main where

import Control.Monad
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Tester

main :: IO ()
main = do
    putStrLn "Server is running on port 8080"
    run 8080 $ \req resp -> do
        undefined
