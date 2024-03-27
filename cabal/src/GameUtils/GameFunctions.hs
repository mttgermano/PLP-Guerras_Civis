module GameFunctions where
import GameFunctionsInit
import DbFunctions

import qualified Data.ByteString.Char8 as BS2
import Control.Concurrent (threadDelay)
import GHC.Generics

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


sleep :: Int -> IO ()
sleep minutes = do
    let delay = minutes * (60 * 1000000)
    threadDelay delay

-- Increment the vote for a user in the db
incrementVote :: String -> String -> IO ()   
incrementVote pName pName_voted = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET votes = votes + 1 WHERE user_id = ?"
    _ <- execute conn sqlQuery (Only pName_voted)
    ----------------------------------------------
    putStrLn $ ("> Voto incrementado para user [" ++ (pName_voted) ++ "]")
    close conn


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


-- Count the number of live players of a role
getPlayerRolesCount :: String -> Bool -> IO Int
getPlayerRolesCount rName isGood = do
    rPlayers    <- getRoomPlayers rName

    -- DB Query ----------------------------------
    total       <- mapM getIsGood rPlayers
    let count   = length $ filter id total 
    ----------------------------------------------
    if isGood
        then return count
        else return (12 - count)


getIsGood :: String -> IO Bool
getIsGood uuid = do
    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT r.isGood FROM UserGameData u INNER JOIN Roles r ON u.role_idx = r.role_idx WHERE u.player_uuid = ?"
    result <- query conn sqlQuery (Only uuid) :: IO [Only Bool]
    ----------------------------------------------
    close conn

    let answer = (fromOnly (head result))
    return answer


getRole :: String -> IO Int
getRole uuid = do
    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT r.role_idx FROM UserGameData u INNER JOIN Roles r ON u.role_idx = r.role_idx WHERE u.player_uuid = ?"
    result <- query conn sqlQuery (Only uuid)
    ----------------------------------------------
    close conn
    case result of
        [Only role]     -> return role
        _               -> return (-1)

getPlayerRoomName :: String -> IO String
getPlayerRoomName pUUID = do
    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT room_name FROM Room WHERE room_uuid = (SELECT current_room FROM Player WHERE player_uuid = ?)"
    [Only rName] <- query conn sqlQuery (Only pUUID) :: IO [Only String]
    ----------------------------------------------
    close conn

    return rName
