module GameFunctions where
import DbFunctions

import GHC.Generics
import Data.Aeson
import Data.List (permutations)

import qualified Data.ByteString.Lazy.Char8 as BS1
import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))



-- User Data Type
data UserGame = UserGame { 
    pId :: String
} deriving (Show, Generic)

-- Convert User into a tuple for SQL insert
instance ToRow UserGame where
    toRow userGame = [toField (pId userGame)]


-- Count the number of live players of a role
getPlayerRolesCount :: Bool -> String -> IO Int
getPlayerRolesCount isGood rName = do
    conn <- getDbConnection
    -- TODO concertar a logica --
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT Player.player_uuid, COUNT(*) \
        \FROM UserGameData \
        \JOIN Player ON UserGameData.player_uuid = Player.player_uuid \
        \JOIN Roles ON UserGameData.role_idx = Roles.role_idx \
        \WHERE current_room = ? AND Roles.isGood = ? AND Roles.role_idx = 0 \
        \GROUP BY Player.player_uuid"
    answer <- execute conn sqlQuery (isGood, rName)
    ----------------------------------------------
    close conn
    return $ fromIntegral answer


-- Increment the vote for a user in the db
incrementVote :: String -> String -> IO ()   
incrementVote pName pName_voted = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET votes = votes + 1 WHERE user_id = ?"
    result <- execute conn sqlQuery (Only pName_voted)
    ----------------------------------------------
    putStrLn $ "> Vote incremented for user [" ++ (pName_voted) ++ "]"
    close conn
