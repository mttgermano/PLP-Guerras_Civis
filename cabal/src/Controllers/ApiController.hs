module Controllers.ApiController where

import Controllers.GameController
import Controllers.RoundController

import GameUtils.GameStartFunctions
import GameUtils.GameStopFunctions
import GameUtils.GameFunctions
import GameUtils.RoleFunctions
import GameUtils.ChatFunctions
import GameUtils.RoundUtils
import GameUtils.BotLogic

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
getRoomPlayers :: String -> IO [(String, String)]
getRoomPlayers rName = do
    pListUUID   <- getRoomPlayersUUIDList rName
    pListNames  <- mapM getPlayerNameFromUUID pListUUID

    let playerList = zip pListUUID pListNames
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
