module UserFunctions where

import Control.Exception (bracket)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

-- User Data Type
data User = User
    { userId :: String,
      userName :: String,
      password :: String
    }
    deriving (Show)

-- Convert User into a tuple for SQL insert
instance ToRow User where
    toRow user = [toField (userId user), toField (userName user), toField (password user)]

-- Create a user in the db
createUser :: User -> IO ()
createUser user = do
    uuid <- fmap toString nextRandom    -- Generate random UUID
    conn <- getDbConnection

    -- DB Query ----------------------------------
    execute conn "INSERT INTO users (user_id, user_name, password) VALUES (?, ?, ?)" user
    ----------------------------------------------
    close conn

-- Establish a database connection
getDbConnection :: IO Connection
getDbConnection = connectPostgreSQL "host=localhost dbname=mydatabase user=myuser password=mypassword"
