{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Lens
import Data.ByteString.Lens
import Data.Text.Lens
import qualified Data.Map as M
import Data.Aeson.Lens
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
              flip runContT (pure . review _VRespReg) $ do
                  callCC $ \resp -> do
                      let (email, pass) = body ^. _VUser

                      lift (getUser email) >>= \x -> when (has _Just x) $ resp RegEmailInUse

                      res <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy $ pass ^. packedChars
                      hashed <- maybe (lift $ throwError err500) (pure . view unpackedChars) res

                      user <- lift $ createUser email hashed

                      token <- liftIO $ generateToken $ user ^. _userId
                      pure $ RegOk token

          loginH body = do
              flip runContT (pure . review _VRespLogin) $ do
                  callCC $ \resp -> do
                      let (email, pass) = body ^. _VUser

                      user <- lift (getUser email) >>= maybe (resp LoginWrongCredentials) pure

                      unless (validatePassword (user ^. _pass . packedChars) (pass ^. packedChars)) $ do
                          void $ resp LoginWrongCredentials

                      token <- liftIO $ generateToken $ user ^. _userId
                      pure $ LoginOk token
