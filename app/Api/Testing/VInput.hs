{-# LANGUAGE DeriveGeneric #-}

module Api.Testing.VInput
    ( VInput, _VInput
    ) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Api.Testing.Core

data VInput = VInput { source :: String
                     , lang :: String
                     }
                     deriving Generic

instance FromJSON VInput
instance ToJSON VInput

_VInput :: Prism' VInput (String, Language)
_VInput = prism (uncurry VInput . over _2 showLang) parse
    where parse input@(VInput source lang) = maybe (Left input) (\x -> Right (source, x)) $ parseLang lang

          parseLang "Haskell" = Just Haskell
          parseLang "C" = Just C
          parseLang "Cpp" = Just Cpp
          parseLang "Python" = Just Python
          parseLang _ = Nothing

          showLang Haskell = "Haskell"
          showLang C = "C"
          showLang Cpp = "Cpp"
          showLang Python = "Python"
