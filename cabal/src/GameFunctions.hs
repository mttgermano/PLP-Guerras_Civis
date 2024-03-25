module GameFunctions where
import GameRoundFunctions
import DbFunctions

import GHC.Generics

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))



-- User Data Type
data UserGame = UserGame { 
    pId_ :: String
} deriving (Show, Generic)

-- Convert User into a tuple for SQL insert
instance ToRow UserGame where
    toRow userGame = [toField (pId_ userGame)]


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
    putStrLn $ ("> Voto incrementado para user [" ++ (pName_voted) ++ "]")
    close conn


-- Return a list with the players names
getPlayersNames :: [String] -> IO [String]
getPlayersNames [] = return []
getPlayersNames (id:ids) = do
    maybeName <- getPlayerFromID (show id)
    rest <- getPlayersNames ids
    return $ case maybeName of
        Just name -> name : rest
        Nothing   -> rest



isPlayerAlive ::  String -> IO Bool
isPlayerAlive playerUuid = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT is_alive FROM UserGameData WHERE player_uuid = ?"    
    result <- query conn sqlQuery (Only playerUuid)
    ----------------------------------------------
    close conn
    case result of
        [Only alive] -> return alive
        _            -> return False


getIsGood :: String -> IO Bool
getIsGood uuid = do
    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT r.isGood FROM UserGameData u INNER JOIN Roles r ON u.role_idx = r.role_idx WHERE u.player_uuid = ?"
    result <- query conn sqlQuery (Only uuid)
    ----------------------------------------------
    close conn
    case result of
        [Only alive] -> return alive
        _            -> return False


getRole :: String -> IO Int
getRole uuid = do
    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT r.role_idx FROM UserGameData u INNER JOIN Roles r ON u.role_idx = r.role_idx WHERE u.player_uuid = ?"
    result <- query conn sqlQuery (Only uuid)
    ----------------------------------------------
    close conn
    case result of
        [Only role] -> return role
        _            -> return (-1)