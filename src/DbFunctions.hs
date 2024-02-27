module DbFunctions where

import qualified Data.ByteString.Char8 as BS2
import Database.PostgreSQL.Simple

-- Establish a database connection
getDbConnection :: IO Connection
getDbConnection = connectPostgreSQL $ BS2.pack "host=localhost dbname=mydatabase user=myuser password=mypassword"
