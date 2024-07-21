{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.User where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
import Database.HDBC

import Database.Monad

data User = User { __userId :: Maybe Int
                 , __email :: String
                 , __pass :: String
                 }
                 deriving Generic

instance FromJSON User
instance ToJSON User

makeLenses ''User

getUser :: String -> DB (Maybe User)
getUser email = do
    c <- ask
    row <- liftIO $ do
        st <- prepare c "SELECT user_id, password FROM users WHERE email = ?"
        execute st [toSql email]
        fetchRow st

    case row of
        Just [userId, pass] -> pure $ Just $ User (Just $ fromSql userId) email (fromSql pass)
        Nothing -> pure Nothing
        _ -> throwError err500

createUser :: User -> DB Int
createUser user = do
    c <- ask
    row <- liftIO $ withTransaction c $ \c -> do
        st <- prepare c "INSERT INTO users (email, password) VALUES (?, ?)"
        execute st [toSql $ user ^. _email, toSql $ user ^. _pass]

        st <- prepare c "SELECT userId FROM users WHERE email = ? AND password = ?"
        execute st [toSql $ user ^. _email, toSql $ user ^. _pass]
        fetchRow st

    case row of
        Just [userId] -> pure $ fromSql userId
        _ -> throwError err500