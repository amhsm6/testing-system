{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Configuration.Dotenv
import Servant
import Network.Wai.Handler.Warp

import Api.Testing
import Api.Course
import Api.User
import DB

type Api = TestApi :<|> CourseApi :<|> UserApi

api :: Proxy Api
api = Proxy

server :: ServerT Api DB
server = testService :<|> courseService :<|> userService

main :: IO ()
main = do
    loadFile defaultConfig
    putStrLn "Server is running on port 3000"
    run 3000 $ serve api $ hoistServer api runDB server
