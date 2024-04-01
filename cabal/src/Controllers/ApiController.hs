module Controllers.ApiController where

import Controllers.GameController
import Controllers.RoundController

import Game.StartFunctions
import Game.StopFunctions
import Game.GameFunctions
import Game.RoleFunctions
import Game.ChatFunctions
import Game.BotLogic
import Utils.Utils

import Core.DbFunctions

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.Types (Query(Query))



-- Define a data type for room information
data RoomData = RoomData {
    rName   :: String,
    rMaster :: String,
    rIsUp   :: Bool
}

data KnowledgeData = KnowledgeData {
    pList       :: [String],
    roleList    :: [String]
}

data ActionsData = ActionsData {
    paList      :: [String],
    actionsList :: [String]
}

-- Get the room infos
getRoomData :: String -> IO [RoomData]
getRoomData rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT room_name, room_master, is_up FROM Room WHERE room_name = ?"
    result <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
    return $ map (\(name, master, isUp) -> RoomData name master isUp) result


-- Get the list of all Rooms
getRoomList :: IO [(String, Int)]
getRoomList = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT room_name FROM Room"
    result <- query_ conn sqlQuery :: IO [Only String]
    ----------------------------------------------
    close conn

    -- Fetch players count for each room
    roomPlayersCounts <- mapM getRoomPlayersCount (map fromOnly result)

    -- Combine room names with their player counts into tuples
    let roomList = zip (map fromOnly result) roomPlayersCounts

    return roomList


-- Get the players of a Room
getRoomPlayers :: String -> IO [(String, String, Bool)]
getRoomPlayers rName = do
    pListUUID   <- getRoomPlayersUUIDList       rName
    pListNames  <- mapM getPlayerNameFromUUID   pListUUID
    pAliveList  <- mapM isPlayerAlive           pListUUID

    let playerList = zip3 pListUUID pListNames pAliveList
    return playerList
 

-- Get the Round State
getRoomState :: String -> IO String
getRoomState rName = getRoomRoundState rName


-- Get the Room messages 
getRoomMessages :: String -> Int -> IO [String] 
getRoomMessages rName lastIdxPlayer = do
    lastIdxServer <- getLastMessageIdx rName

    if lastIdxPlayer /= lastIdxPlayer
        then do
            lastMessages <- getMessagesListFromRoom rName lastIdxPlayer
            return lastMessages
        else
            return [""]


getPlayerKnowledge :: String -> IO [KnowledgeData]
getPlayerKnowledge pName = do
    pUUID           <- getUUIDFromPlayerName pName
    pKnowUUIDList   <- getPlayerKnowledgeList pUUID

    pNameList       <- mapM getPlayerNameFromUUID pKnowUUIDList
    pRoleIdxList    <- mapM getRole     pKnowUUIDList
    pRoleNameList   <- mapM getRoleName pRoleIdxList
    
    let knowledgeDataList = zipWith (\pName pRoleName -> KnowledgeData { pList = [pName], roleList = [pRoleName] }) pNameList pRoleNameList
    return knowledgeDataList


createListWithWord :: [String] -> String -> [String]
createListWithWord listt word = replicate (length listt) word

getRoomActionsResults :: String -> IO [ActionsData]
getRoomActionsResults rName = do
    pListUUID   <- getRoomPlayersUUIDList       rName
    pListNames  <- mapM getPlayerNameFromUUID   pListUUID
    pAliveList  <- mapM isPlayerAlive           pListUUID

    silencedPlayers     <- getRoundPlayersRecieved rName "is_silenced"
    paralizedPlayers    <- getRoundPlayersRecieved rName "is_paralized"
    killedPlayers       <- getRoundPlayersRecieved rName "kill_vote"
     
    let playerDataList  = silencedPlayers ++ paralizedPlayers ++ killedPlayers
    let actionDataList  = (createListWithWord silencedPlayers "silenciado") ++ (createListWithWord paralizedPlayers "paralizado") ++ (createListWithWord killedPlayers "morreu")
    
    let actionsData = zipWith (\players actions -> ActionsData { paList = [players], actionsList = [actions] }) playerDataList actionDataList
    return actionsData


getRoundPlayersRecieved :: String -> String -> IO [String]
getRoundPlayersRecieved rName action = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack ("SELECT player_uuid FROM UserGameData WHERE " ++ action ++ " > 0")
    [Only result] <- query conn sqlQuery (Only rName) :: IO [Only String]
    ----------------------------------------------
    close conn

    return [result]
