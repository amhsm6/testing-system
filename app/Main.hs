{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Configuration.Dotenv
import Servant
import Network.Wai.Handler.Warp

import Api.Testing
import Api.Course
import Api.User
import Api.Static
import DB

type Api = TestApi :<|> CourseApi :<|> UserApi :<|> StaticApi

api :: Proxy Api
api = Proxy

server :: ServerT Api DB
server = testService :<|> courseService :<|> userService :<|> staticService

main :: IO ()
main = do
    loadFile defaultConfig
    putStrLn "Server is running on port 8080"
    run 8080 $ serve api $ hoistServer api runDB server
