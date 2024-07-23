{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.User where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Data.ByteString.Lens
import Data.Text.Lens
import Data.Aeson.Lens
import qualified Data.Map as M
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Environment
import Web.JWT
import Servant
import Database.HDBC

import DB

data User = User { __userId :: Int
                 , __email :: String
                 , __pass :: String
                 }

makeLenses ''User

type CreateUser = String

data RegResp = RegEmailInUse
             | RegOk { regToken :: String }
             deriving Generic

instance FromJSON RegResp
instance ToJSON RegResp

data AuthResp = AuthWrongCredentials
              | AuthOk { authToken :: String }
              deriving Generic

instance FromJSON AuthResp
instance ToJSON AuthResp

generateToken :: Int -> IO String
generateToken userId = do
    secret <- liftIO $ getEnv "JWT_SECRET"
    let payload = [("userId", userId)] & traverse . _1 %~ view packed
                                       & traverse . _2 %~ review _JSON
        claims = mempty { unregisteredClaims = ClaimsMap $ M.fromList payload
                        }
        token = encodeSigned (EncodeHMACSecret $ secret ^. packedChars) mempty claims
    pure $ token ^. unpacked

getUser :: String -> DB (Maybe User)
getUser email = do
    c <- ask
    row <- liftIO $ do
        st <- prepare c "SELECT user_id, password FROM users WHERE email = ?"
        execute st [toSql email]
        fetchRow st

    case row of
        Just [userId, pass] -> pure $ Just $ User (fromSql userId) email (fromSql pass)
        Nothing -> pure Nothing
        _ -> throwError err500

createUser :: CreateUser -> DB Int
createUser dat = do
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