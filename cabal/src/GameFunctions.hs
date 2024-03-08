module GameFunctions where
import DbFunctions

import GHC.Generics
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


-- Increment the vote for a user in the db
incrementVote :: BS1.ByteString -> IO ()
incrementVote jsonRequest = do
    let maybeUser = parseUserRequest jsonRequest

    case maybeUser of
        Just user -> do
            conn <- getDbConnection

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "UPDATE votes SET number_of_votes = number_of_votes + 1 WHERE user_id = ?"
            result <- execute conn sqlQuery (uName user, uPassword user)
            ----------------------------------------------
            close conn

            putStrLn $ "Vote incremented for user " ++ (uName user)

        Nothing ->
            putStrLn "Invalid JSON format."


-- Parse a POST request
parseUserRequest :: BS1.ByteString -> Maybe User
parseUserRequest = decode
