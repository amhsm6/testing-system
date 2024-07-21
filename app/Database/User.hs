{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.User where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
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

makeLenses ''User

checkUser :: String -> DB Bool
checkUser email = do
    c <- ask
    liftIO $ do
        st <- prepare c "SELECT * FROM users WHERE email = ?"
        execute st [toSql email]
        has _Just <$> fetchRow st

createUser :: User -> DB Int
createUser user = do
    c <- ask
    liftIO $ withTransaction c $ \c -> do
        st <- prepare c "INSERT INTO users (email, password) VALUES (?, ?)"
        execute st [toSql $ user ^. _email, toSql $ user ^. _pass]

    validateUser user >>= maybe (throwError err500) pure

validateUser :: User -> DB (Maybe Int)
validateUser user = do
    c <- ask
    row <- liftIO $ do
        st <- prepare c "SELECT user_id FROM users WHERE email = ? AND password = ?"
        execute st [toSql $ user ^. _email, toSql $ user ^. _pass]
        fetchRow st

    case row of
        Just [userId] -> pure $ Just $ fromSql userId
        Nothing -> pure Nothing
        _ -> throwError err500
