{-# LANGUAGE DeriveGeneric #-}

module Api.Testing.VInput
    ( VInput, vinput
    ) where

import Control.Monad
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data VInput = VInput { source :: String
                     , lang :: String
                     }
                     deriving Generic

instance FromJSON VInput
instance ToJSON VInput

vinput :: Prism' VInput (String, Language)
vinput = prism (uncurry VInput) $ \(VInput source lang) -> parseLang
