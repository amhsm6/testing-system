{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.User where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Data.ByteString.Lens
import Data.Aeson
import GHC.Generics
import Servant
import Database.HDBC

import Database.Monad

data User = User { __email :: String
                 , __pass :: String
                 }
                 deriving Generic

instance FromJSON User
instance ToJSON User

checkUser :: String -> DB Bool
checkUser = error "should check if user with that email exists"

createUser :: User -> DB ()
createUser = error "should create user"

validateUser :: User -> DB Bool
validateUser = error "should check user email and password"
