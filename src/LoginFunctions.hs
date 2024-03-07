{-# LANGUAGE DeriveGeneric #-}
module LoginFunctions (createUser, loginUser) where
import DbFunctions

import GHC.Generics
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))



-- User Data Type
data User = User
    { uId :: String,
      uName :: String,
      uPassword :: String
    }
    deriving (Show, Generic)

-- Convert User into a tuple for SQL insert
instance ToRow User where
    toRow user = [toField (uId user), toField (uName user), toField (uPassword user)]

-- Create a user in the db
createUser :: String -> String -> IO ()
createUser user_name user_password = do
    uuid <- fmap toString nextRandom    -- Generate random UUID
    conn <- getDbConnection

    let newUser = User { uId = uuid, uName = user_name, uPassword = user_password}

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "INSERT INTO users (user_id, user_name, password) VALUES (?, ?, ?)"
    result <- execute conn sqlQuery newUser
    print result
    ----------------------------------------------
    close conn
    putStrLn $ "User created: " ++ show newUser


-- Log in a user
loginUser :: String -> String -> IO ()
loginUser user_name user_password = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT user_id FROM users WHERE user_name = ? AND user_password = ?"
    result <- query conn sqlQuery (user_name, user_password) :: IO [Only String]
    ----------------------------------------------
    close conn

    -- Check if the query returned any rows
    if null result
        then putStrLn "Invalid username or password."
        else putStrLn "Login successful."
