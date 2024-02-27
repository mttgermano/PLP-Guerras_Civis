{-# LANGUAGE DeriveGeneric #-}
module LoginFunctions (createUser) where
import DbFunctions

import GHC.Generics
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BS1
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

-- Automatically derive FromJSON instance
instance FromJSON User

-- Convert User into a tuple for SQL insert
instance ToRow User where
    toRow user = [toField (uId user), toField (uName user), toField (uPassword user)]

-- Create a user in the db
createUser :: BS1.ByteString -> IO ()
createUser jsonRequest = do
    let maybeUser = parseUserRequest jsonRequest

    case maybeUser of
        Just user -> do
            uuid <- fmap toString nextRandom    -- Generate random UUID
            conn <- getDbConnection

            let newUser = user { uId = uuid }

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "INSERT INTO users (user_id, user_name, password) VALUES (?, ?, ?)"
            result <- execute conn sqlQuery newUser
            ----------------------------------------------
            close conn
            putStrLn $ "User created: " ++ show newUser

        Nothing ->
            putStrLn "Invalid JSON format."


-- Log in a user
login :: BS1.ByteString -> IO ()
login jsonRequest = do
    let maybeUser = parseUserRequest jsonRequest

    case maybeUser of
        Just user -> do
            conn <- getDbConnection

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "SELECT user_id FROM users WHERE user_name = ? AND user_password = ?"
            result <- query conn sqlQuery (uName user, uPassword user) :: IO [Only String]
            ----------------------------------------------
            close conn

            -- Check if the query returned any rows
            if null result
                then putStrLn "Invalid username or password."
                else putStrLn "Login successful."

        Nothing ->
            putStrLn "Invalid JSON format."


-- Parse a POST request
parseUserRequest :: BS1.ByteString -> Maybe User
parseUserRequest = decode
