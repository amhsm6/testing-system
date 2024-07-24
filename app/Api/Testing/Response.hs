module Api.Testing.Response where

import Api.Testing.Core

data Response = ResponsePassed
              | ResponseLogs [String]
              | ProblemDoesNotExist
              | UnsupportedLanguage
              | TestingError TestError
