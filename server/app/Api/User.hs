{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens
import Data.ByteString.Lens
import Data.Text.Lens
import Data.Aeson.Lens
import qualified Data.Map as M
import System.Environment
import Crypto.BCrypt
import Web.JWT
import Servant

import Api.User.RespReg
import Api.User.RespLogin
import Api.User.VUser
import Api.User.VRespReg
import Api.User.VRespLogin
import DB
import DB.User

type UserApi = "api" :> "register" :> ReqBody '[JSON] VUser :> Post '[JSON] VRespReg
          :<|> "api" :> "login" :> ReqBody '[JSON] VUser :> Post '[JSON] VRespLogin

generateToken :: Int -> IO String
generateToken userId = do
    secret <- liftIO $ getEnv "JWT_SECRET"
    let payload = [("userId", userId)] & traverse . _1 %~ view packed
                                       & traverse . _2 %~ review _JSON
        claims = mempty { unregisteredClaims = ClaimsMap $ M.fromList payload
                        }
        token = encodeSigned (EncodeHMACSecret $ secret ^. packedChars) mempty claims

    pure $ token ^. unpacked

userService :: ServerT UserApi DB
userService = regH :<|> loginH
    where regH body = do
              runExceptT >=> pure . review _VRespReg . either id id $ do
                  let (email, pass) = body ^. _VUser

                  lift (getUser email) >>= \x -> when (has _Just x) $ throwError RegEmailInUse

                  res <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy $ pass ^. packedChars
                  hashed <- maybe (lift $ throwError err500) (pure . view unpackedChars) res

                  user <- lift $ createUser email hashed

                  token <- liftIO $ generateToken $ user ^. _userId
                  pure $ RegOk token

          loginH body = do
              runExceptT >=> pure . review _VRespLogin . either id id $ do
                  let (email, pass) = body ^. _VUser

                  user <- lift (getUser email) >>= maybe (throwError LoginWrongCredentials) pure

                  unless (validatePassword (user ^. _pass . packedChars) (pass ^. packedChars)) $ do
                      throwError LoginWrongCredentials

                  token <- liftIO $ generateToken $ user ^. _userId
                  pure $ LoginOk token
