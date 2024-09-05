{-# LANGUAGE TemplateHaskell #-}

module DB.User where

import Control.Monad.Reader
import Control.Lens
import Servant
import Database.HDBC

import DB

data User = User { __userId :: Int
                 , __email :: String
                 , __pass :: String
                 }

makeLenses ''User

getUser :: Int -> DB (Maybe User)
getUser userId = do
    c <- ask
    row <- liftIO $ do
        st <- prepare c "SELECT email, password FROM users WHERE user_id = ?"
        execute st [toSql userId]
        fetchRow st

    case row of
        Just [email, pass] -> pure $ Just $ User userId (fromSql email) (fromSql pass)
        Nothing -> pure Nothing
        _ -> throwError err500

getUserByEmail :: String -> DB (Maybe User)
getUserByEmail email = do
    c <- ask
    row <- liftIO $ do
        st <- prepare c "SELECT user_id, password FROM users WHERE email = ?"
        execute st [toSql email]
        fetchRow st

    case row of
        Just [userId, pass] -> pure $ Just $ User (fromSql userId) email (fromSql pass)
        Nothing -> pure Nothing
        _ -> throwError err500

createUser :: String -> String -> DB User
createUser email pass = do
    c <- ask
    row <- liftIO $ withTransaction c $ \c -> do
        st <- prepare c "INSERT INTO users (email, password) VALUES (?, ?)"
        execute st [toSql email, toSql pass]

        st <- prepare c "SELECT user_id FROM users WHERE email = ? AND password = ?"
        execute st [toSql email, toSql pass]
        fetchRow st

    case row of
        Just [userId] -> pure $ User (fromSql userId) email pass
        _ -> throwError err500
