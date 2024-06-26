module Utils.Utils where

import Core.DbFunctions

import qualified Data.ByteString.Char8 as BS2
import Data.Maybe (isJust)


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
    pName           <- getPlayerNameFromUUID id
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
    rPlayers <- getRoomPlayersUUIDList rName

    -- DB Query ----------------------------------
    let queryFunction puuid = do
            isAlive <- isPlayerAlive puuid -- Assuming getPlayerIsAlive returns a boolean indicating whether a player is alive or not
            if isAlive
                then do
                    isGoodValue <- getIsGood puuid
                    return (isGoodValue == isGood)
                else return False
    total <- mapM queryFunction rPlayers
    let count = length $ filter id total
    ----------------------------------------------

    return count


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

-- Get the role idx - using the its name
getRoleIdx :: String -> IO Int
getRoleIdx roleName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT role_idx FROM Roles WHERE role = ?"
    [Only result] <- query conn sqlQuery (Only roleName) :: IO [Only Int]
    ----------------------------------------------
    close conn

    return result


getPlayerKnowledgeList :: String -> IO [String]
getPlayerKnowledgeList pUUID = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT who_is_known FROM RoleKnowledge WHERE who_knows = ?"
    results <- query conn sqlQuery (Only pUUID) :: IO [Only String]
    ----------------------------------------------
    close conn

    return (map (\(Only str) -> str) results)

-- Get the player UUID - using its roleIdx
getPlayerUUIDFromRoleIdx :: Int -> IO String
getPlayerUUIDFromRoleIdx roleIdx = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT player_uuid FROM UserGameData WHERE role_idx = ?"
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
deleteRoomPlayersKnowledge :: String -> IO ()
deleteRoomPlayersKnowledge rName = do
    pList   <- getRoomPlayersUUIDList rName
    mapM_ deletePLayerKnowledge pList 

    
-- Delete a player knowlegde
deletePLayerKnowledge :: String -> IO ()
deletePLayerKnowledge pUUID = do
    conn    <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "DELETE FROM RoleKnowledge WHERE who_knows = ?"
    _ <- execute conn sqlQuery (Only pUUID)
    ----------------------------------------------
    close conn

isRoleAlive :: [String] -> Int -> IO Bool
isRoleAlive [] _ = return False
isRoleAlive (playerId:rest) role = do
    isAlive     <- isPlayerAlive playerId
    playerRole  <- getRole playerId

    if isAlive && playerRole == role
        then return True
        else isRoleAlive rest role

-- Check if the player can do the action
isAllowed :: String -> String -> IO Bool
isAllowed pName actionType = do
    pUUID       <- getUUIDFromPlayerName    pName
    pRoom       <- getPlayerRoomName        pUUID
    rState      <- getRoomRoundState        pRoom
    paralized   <- isParalized              pUUID
    silenced    <- isSilenced               pUUID
    isAlive     <- isPlayerAlive            pUUID

    if silenced > 0 || not isAlive || paralized > 0 || (actionType /= rState)
        then return False 
    else        -- default
        return True


-- Get a int representing if the player is silenced 
isSilenced :: String -> IO Int
isSilenced pUUID = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT is_silenced FROM UserGameData WHERE player_uuid = ?"    
    [Only result] <- query conn sqlQuery (Only pUUID)
    ----------------------------------------------
    close conn
    return result


-- Get a int representing if the player is paralized
isParalized :: String -> IO Int
isParalized pUUID = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT is_paralized FROM UserGameData WHERE player_uuid = ?"    
    [Only result] <- query conn sqlQuery (Only pUUID)
    ----------------------------------------------
    close conn
    return result


-- Get a Bool telling if the player is alive - using its UUID
isPlayerAlive ::  String -> IO Bool
isPlayerAlive pUUID = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT is_alive FROM UserGameData WHERE player_uuid = ?"    
    [Only result] <- query conn sqlQuery (Only pUUID)
    ----------------------------------------------
    close conn

    return result


hasCursedWord :: String -> IO Bool
hasCursedWord pName = do
    maybeCursedWord <- getCursedWord pName
    return $ case maybeCursedWord of
        Just word -> word /= "a"
        _         -> False

getCursedWord :: String -> IO (Maybe String)
getCursedWord rName = do
    conn <- getDbConnection
    let sqlQuery = Query $ BS2.pack "SELECT cursed_word FROM Room WHERE room_name = ?"
    results <- query conn sqlQuery (Only rName) :: IO [Only String]
    close conn
    return $ case results of
        [Only cursedWord] -> Just cursedWord
        _                 -> Nothing


admSendMessage :: String -> String -> IO ()
admSendMessage rName msg = do
    rState  <- getRoomRoundState rName
    conn    <- getDbConnection
    let pName = "Sistema"

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE Room SET round_messages = round_messages || ARRAY[(?, ?)::message_pair] WHERE room_name = ?"
    _ <- execute conn sqlQuery (pName, msg, rName)
    ----------------------------------------------
    close conn

    -- send a request to the react server    
    putStrLn $ "> [" ++ rName ++ "] Room - recebeu mensagem [" ++ msg ++ "] de " ++ pName ++ "]"


getRoomAlivePlayers :: String -> IO [Only String]
getRoomAlivePlayers rName = do
    pListUUID   <- getRoomPlayersUUIDList rName
    aliveStatus <- mapM isPlayerAlive pListUUID

    let alive = map fst $ filter snd $ zip (map Only pListUUID) aliveStatus
    return alive


-- getRoomActionsResults :: String -> IO ()
-- getRoomActionsResults rName = do
--     pListUUID   <- getRoomPlayersUUIDList       rName
--     pListNames  <- mapM getPlayerNameFromUUID   pListUUID
--     pAliveList  <- mapM isPlayerAlive           pListUUID

--     silencedPlayers     <- getRoundPlayersRecieved "is_silenced"
--     paralizedPlayers    <- getRoundPlayersRecieved "is_paralized"
--     killedPlayers       <- getRoundPlayersRecieved "kill_vote"

--     silencedPlayersNames  <- mapM getPlayerNameFromUUID   silencedPlayers
--     paralizedPlayersNames  <- mapM getPlayerNameFromUUID   paralizedPlayers
--     killedPlayersNames  <- mapM getPlayerNameFromUUID   killedPlayers

--     mapM_ (admSendMessage rName) $ map (\player -> player ++ " foi silenciado") silencedPlayersNames
--     mapM_ (admSendMessage rName) $ map (\player -> player ++ " foi paralizado") paralizedPlayersNames
--     mapM_ (admSendMessage rName) $ map (\player -> player ++ " foi morto") killedPlayersNames


-- getRoundPlayersRecieved :: String -> IO [String]
-- getRoundPlayersRecieved action = do
--     conn <- getDbConnection

--     -- DB Query ----------------------------------
--     let sqlQuery = Query $ BS2.pack ("SELECT player_uuid FROM UserGameData WHERE " ++ action ++ " > 0")
--     results <- query_ conn sqlQuery :: IO [Only String]
--     ----------------------------------------------
--     close conn

--     -- Extracting values from results
--     let players = map (\(Only player) -> player) results
--     return players
