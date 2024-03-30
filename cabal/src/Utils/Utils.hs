module Utils.Utils where

import Core.DbFunctions

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))



-- It selects the itens of the first list whom index, in the second list is equal to flag
selectTheIndex :: [String] -> [Bool] -> Bool -> [String]
selectTheIndex pList gList b = [x | (x, flag) <- zip pList gList, flag == b]

-- Get the list of all good players in a room
getRoomPlayersGoodness :: String -> Bool -> IO [String]
getRoomPlayersGoodness rName isGood = do
    rPlayers        <- getRoomPlayersUUIDList rName
    goodnessList    <- mapM getIsGood rPlayers

    return $ selectTheIndex rPlayers goodnessList isGood

-- Get player name from id
getPlayerNameFromUUID :: String -> IO String
getPlayerNameFromUUID pUUID = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT player_name FROM Player WHERE player_uuid = ?"
    [Only pName] <- query conn sqlQuery [pUUID]
    ----------------------------------------------
    close conn
    return pName

-- Get player ID from Name
getUUIDFromPlayerName :: String -> IO String
getUUIDFromPlayerName pName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT player_uuid FROM Player WHERE player_name = ?"
    [Only pUUID] <- query conn sqlQuery [pName]
    ----------------------------------------------
    close conn
    return pUUID

-- Return a list with the players names
getPlayersNames :: [String] -> IO [String]
getPlayersNames [] = return []
getPlayersNames (id:ids) = do
    pName           <- getPlayerNameFromUUID (show id)
    remainingStr    <- getPlayersNames ids

    return (pName : remainingStr)


-- Get the room State
getRoomRoundState :: String -> IO String
getRoomRoundState rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT round_state FROM Room WHERE room_name = ?"
    [Only result] <- query conn sqlQuery (Only rName) :: IO [Only String]
    ----------------------------------------------
    close conn

    return result

-- Function to get number of players for a room
getRoomPlayersCount :: String -> IO Int
getRoomPlayersCount rName = do
    roomPlayersUUIDList <- getRoomPlayersUUIDList rName
    return $ length roomPlayersUUIDList


-- Count the number of live players of a role
getPlayerRolesCount :: String -> Bool -> IO Int
getPlayerRolesCount rName isGood = do
    rPlayers    <- getRoomPlayersUUIDList rName

    -- DB Query ----------------------------------
    total       <- mapM getIsGood rPlayers
    let count   = length $ filter id total 
    ----------------------------------------------
    if isGood
        then return count
        else return (12 - count)


-- Get a Bool telling if the Player is good or not, using its UUID
getIsGood :: String -> IO Bool
getIsGood pUUID = do
    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT r.isGood FROM UserGameData u INNER JOIN Roles r ON u.role_idx = r.role_idx WHERE u.player_uuid = ?"
    result <- query conn sqlQuery (Only pUUID) :: IO [Only Bool]
    ----------------------------------------------
    close conn

    let answer = (fromOnly (head result))
    return answer


-- Get the role of a player, using its UUID
getRole :: String -> IO Int
getRole pUUID = do
    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT r.role_idx FROM UserGameData u INNER JOIN Roles r ON u.role_idx = r.role_idx WHERE u.player_uuid = ?"
    result <- query conn sqlQuery (Only pUUID)
    ----------------------------------------------
    close conn
    case result of
        [Only role]     -> return role
        _               -> return (-1)

-- Get the role name - using its idx
getRoleName :: Int -> IO String
getRoleName roleIdx = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT role FROM Roles WHERE role_idx = ?"
    [Only result] <- query conn sqlQuery (Only roleIdx) :: IO [Only String]
    ----------------------------------------------
    close conn

    return result


-- Get the Room Name of a player, using its uuid
getPlayerRoomName :: String -> IO String
getPlayerRoomName pUUID = do
    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT room_name FROM Room WHERE room_uuid = (SELECT current_room FROM Player WHERE player_uuid = ?)"
    [Only rName] <- query conn sqlQuery (Only pUUID) :: IO [Only String]
    ----------------------------------------------
    close conn

    return rName

-- Get the list of Bots UUID that are present in a room
getRoomBots :: String -> IO [String]
getRoomBots rName = do
    conn    <- getDbConnection
    rUUID   <- getRoomUuid rName

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT player_uuid FROM Player WHERE current_room = ? AND is_bot = true"    
    result <- query conn sqlQuery (Only rUUID)
    ----------------------------------------------
    close conn
    return $ map (\(Only player_uuid) -> player_uuid) result

-- Get the UUID of a Room
getRoomUuid :: String -> IO String
getRoomUuid rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT room_uuid FROM Room WHERE room_name = ?"    
    [Only result] <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn

    return result

-- Get the State of a Room
getRoomUpState :: String -> IO Bool
getRoomUpState rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT is_up FROM Room WHERE room_name = ?"    
    [Only result] <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
    return result


-- Get all Players UUID from a Room
getRoomPlayersUUIDList :: String -> IO [String]
getRoomPlayersUUIDList rName = do
    conn    <- getDbConnection
    rUuid   <- getRoomUuid rName

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT player_uuid FROM Player WHERE current_room = ?"    
    result <- query conn sqlQuery (Only rUuid)
    ----------------------------------------------
    close conn

    let pList = (map (\(Only player_uuid) -> player_uuid) result)
    return pList

-- Delete all room players knowlegde
deleteRoomPlayersKnowledge :: IO ()
deleteRoomPlayersKnowledge rName = do
    pList   <- getRoomPlayersUUIDList rName
    mapM deletePLayerKnowledge pList

    
-- Delete a player knowlegde
deletePLayerKnowledge :: IO ()
deletePLayerKnowledge pUUID = do
    conn    <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "DELETE FROM RoleKnowledge WHERE who_knows = ?"
    _ <- execute conn sqlQuery (Only pUUID)
    ----------------------------------------------
    close conn
