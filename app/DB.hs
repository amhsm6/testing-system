module DB where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import System.Environment
import Servant
import Database.HDBC.PostgreSQL

type DB = ReaderT Connection Handler

runDB :: DB a -> Handler a
runDB m = do
    let cfg = "host = +DBHOST user = +DBUSER password = +DBPASS dbname = +DBNAME"
    conn <- liftIO $ mapMOf (worded . filtered (elem '+')) (getEnv . drop 1) cfg >>= connectPostgreSQL
    runReaderT m conn
