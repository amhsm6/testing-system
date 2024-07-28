{-# LANGUAGE DeriveGeneric #-}

module Api.User.VUser
    ( VUser, _VUser
    ) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data VUser = VUser { email :: String
                   , pass :: String
                   }
                   deriving Generic

instance FromJSON VUser
instance ToJSON VUser

_VUser :: Iso' VUser (String, String)
_VUser = iso parse (uncurry VUser)
    where parse (VUser email pass) = (email, pass)
