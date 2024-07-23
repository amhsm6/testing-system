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

          regH userIn = undefined {-do
              userExists <- has _Just <$> getUser (userIn ^. _email)
              if userExists then do
                  pure RegEmailInUse
              else do
                  user <- forMOf (_pass . packedChars) userIn $ \pass -> do
                      hashed <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy pass
                      maybe (throwError err500) pure hashed

                  -- userId <- createUser user
                  let userId = 1509

                  liftIO (generateToken userId) >>= pure . RegOk-}

          authH userIn = undefined {-do
              res <- getUser $ userIn ^. _email
              let validUser = do
                      user <- res
                      guard $ validatePassword (user ^. _pass . packedChars) (userIn ^. _pass . packedChars)
                      Just user

              case validUser of
                  Just user -> liftIO (generateToken $ user ^?! _userId) >>= pure . AuthOk
                  Nothing -> pure AuthWrongCredentials-}

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
