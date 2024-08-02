module Api.User.RespLogin where

data RespLogin = LoginOk String
               | LoginWrongCredentials
