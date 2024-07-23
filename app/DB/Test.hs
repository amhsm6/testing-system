{-# LANGUAGE TemplateHaskell #-}

module DB.Test where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Servant
import Database.HDBC

import DB

data Test = Test { __testId :: Int
                 , __input :: String
                 , __output :: String
                 }

makeLenses ''Test

getTests :: Int -> DB [Test]
getTests problemId = do
    c <- ask
    rows <- liftIO $ do
        st <- prepare c "SELECT test_id, input, output FROM tests WHERE problem_id = ? ORDER BY test_id"
        execute st [toSql problemId]
        fetchAllRows st

    forM rows $ \row -> case row of
                            [testId, input, output] -> pure $ Test (fromSql testId) (fromSql input) (fromSql output)
                            _ -> throwError err500
