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
getRoomList :: IO [String]
getRoomList = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT room_name FROM Room"
    result <- query_ conn sqlQuery :: IO [Only String]
    ----------------------------------------------
    close conn
    return $ map fromOnly result


-- Get the players of a Room
getRoomPlayers :: String -> IO [String]
getRoomPlayers rName = do
    pListUUID   <- getRoomPlayersUUIDList rName
    pListNames  <- mapM getPlayerNameFromUUID pListUUID

    return pListNames
