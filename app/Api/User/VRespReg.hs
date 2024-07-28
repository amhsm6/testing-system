{-# LANGUAGE DeriveGeneric #-}

module Api.User.VRespReg
    ( VRespReg, _VRespReg
    ) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Api.User.RespReg

data VRespReg = VRespReg { ok :: Bool
                         , token :: Maybe String
                         , err :: Maybe VRespRegError
                         }
                         deriving Generic

data VRespRegError = VRespRegError { errorCode :: Int }
    deriving Generic

instance FromJSON VRespRegError
instance ToJSON VRespRegError

instance FromJSON VRespReg
instance ToJSON VRespReg

_VRespReg :: Prism' VRespReg RespReg
_VRespReg = prism mapResp Left
    where mapResp (RegOk token) = VRespReg True (Just token) Nothing
          mapResp RegEmailInUse = VRespReg False Nothing (Just $ VRespRegError 1)
