module LoginFunctions where

import Control.Exception (bracket)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BS1
import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query)


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

    putStrLn $ "User created: " ++ show user


-- Log in a user
login :: String -> String -> IO ()
login userName userPassword = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = "SELECT user_id FROM users WHERE user_name = ? AND password = ?" :: Query
    result <- query conn sqlQuery (userName, userPassword) :: IO [Only String]
    ----------------------------------------------
    close conn

    -- Check if the query returned any rows
    if null result
        then putStrLn "Invalid username or password."
        else putStrLn "Login successful."


-- Establish a database connection
getDbConnection :: IO Connection
getDbConnection = connectPostgreSQL $ BS2.pack "host=localhost dbname=mydatabase user=myuser password=mypassword"
