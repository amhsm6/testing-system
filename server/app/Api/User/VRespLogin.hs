{-# LANGUAGE DeriveGeneric #-}

module Api.User.VRespLogin
    ( VRespLogin, _VRespLogin
    ) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Api.User.RespLogin

data VRespLogin = VRespLogin { ok :: Bool
                             , token :: Maybe String
                             , err :: Maybe VRespLoginError
                             }
                             deriving Generic

data VRespLoginError = VRespLoginError { errorCode :: Int }
    deriving Generic

instance FromJSON VRespLoginError
instance ToJSON VRespLoginError

instance FromJSON VRespLogin
instance ToJSON VRespLogin

_VRespLogin :: Prism' VRespLogin RespLogin
_VRespLogin = prism mapResp Left
    where mapResp (LoginOk token) = VRespLogin True (Just token) Nothing
          mapResp LoginWrongCredentials = VRespLogin False Nothing (Just $ VRespLoginError 1)
