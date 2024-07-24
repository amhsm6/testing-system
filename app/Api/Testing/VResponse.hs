{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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

data VResponseError = VResponseError { problemDoesNotExist :: Maybe ()
                                     , unsupportedLanguage :: Maybe ()
                                     , testingError :: Maybe VTestError
                                     }
                                     deriving Generic

data VTestError = VTestError { unknownError :: Maybe ()
                             , compileError :: Maybe ()
                             , wrongAnswer :: Maybe VTest
                             , runtimeError :: Maybe VTest
                             }
                             deriving Generic

data VTest = VTest { input :: String
                   , output :: String
                   }
                   deriving Generic

instance FromJSON VTest
instance ToJSON VTest

instance FromJSON VTestError
instance ToJSON VTestError

instance FromJSON VResponseError
instance ToJSON VResponseError

instance FromJSON VResponse
instance ToJSON VResponse

_VResponse :: Prism' VResponse Response
_VResponse = prism mapResponse Left
    where mapResponse ResponsePassed = VResponse True Nothing Nothing
          mapResponse (ResponseLogs logs) = VResponse True (Just logs) Nothing
          mapResponse x = VResponse False Nothing (Just $ mapRespError x)

          mapRespError ProblemDoesNotExist = VResponseError (Just ()) Nothing Nothing
          mapRespError UnsupportedLanguage = VResponseError Nothing (Just ()) Nothing
          mapRespError (TestingError e) = VResponseError Nothing Nothing (Just $ mapTestError e)
          mapRespError _ = undefined

          mapTestError TestUnknownError = VTestError (Just ()) Nothing Nothing Nothing
          mapTestError TestCompileError = VTestError Nothing (Just ()) Nothing Nothing
          mapTestError (TestWrongAnswerError t) = VTestError Nothing Nothing (Just $ mapTest t) Nothing
          mapTestError (TestRuntimeError t) = VTestError Nothing Nothing Nothing (Just $ mapTest t)

          mapTest t = VTest (t ^. _input) (t ^. _output)
