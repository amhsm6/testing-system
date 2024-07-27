{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Data.Text.Lens
import qualified Data.Map as M
import System.Environment
import Web.JWT
import Servant
import Database.HDBC

import Api.User.VUser
import Api.User.VRespReg
import Api.User.VRespLogin
import DB
import DB.User

type UserApi = "register" :> ReqBody '[JSON] VUser :> POST '[JSON] VRespReg
          :<|> "login" :> ReqBody '[JSON] VUser :> POST '[JSON] VRespLogin

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
              (email, pass) <- maybe (throwError err400) pure $ body ^. pre _VUser

              review _VRespReg <$> do
                  userExists <- has _Just <$> getUser email
                  if userExists then do
                      pure RegEmailInUse
                  else do
                      res <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy $ pass ^. packedChars
                      hashed <- maybe (throwError err500) (pure . view unpackedChars) res

                      -- userId <- createUser email hashed
                      let userId = 1509

                      liftIO (generateToken userId) >>= pure . RegOk

          loginH userIn = do
              (email, pass) <- maybe (throwError err400) pure $ body ^. pre _VUser
