{-# LANGUAGE DeriveGeneric #-}

module Api.Testing.VResponse
    ( VResponse, _VResponse
    ) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Api.Testing.Core
import Api.Testing.Response
import DB.Test

data VResponse = VResponse { ok :: Bool
                           , logs :: Maybe [String]
                           , err :: Maybe VResponseError
                           }
                           deriving Generic

data VResponseError = VResponseError { errorCode :: Int
                                     , errorData :: Maybe VTest
                                     }
                                     deriving Generic

data VTest = VTest { input :: String
                   , output :: String
                   }
                   deriving Generic

instance FromJSON VTest
instance ToJSON VTest

instance FromJSON VResponseError
instance ToJSON VResponseError

instance FromJSON VResponse
instance ToJSON VResponse

_VResponse :: Prism' VResponse Response
_VResponse = prism mapResponse Left
    where mapResponse ResponsePassed = VResponse True Nothing Nothing
          mapResponse (ResponseLogs logs) = VResponse True (Just logs) Nothing
          mapResponse x = VResponse False Nothing (Just $ mapRespError x)

          mapRespError ProblemDoesNotExist = VResponseError 1 Nothing
          mapRespError UnsupportedLanguage = VResponseError 2 Nothing
          mapRespError (TestingError e) = mapTestingError e
          mapRespError _ = undefined

          mapTestingError TestUnknownError = VResponseError 3 Nothing
          mapTestingError TestCompileError = VResponseError 4 Nothing
          mapTestingError (TestWrongAnswerError t) = VResponseError 5 (Just $ mapTest t)
          mapTestingError (TestRuntimeError t) = VResponseError 6 (Just $ mapTest t)

          mapTest t = VTest (t ^. _input) (t ^. _output)
