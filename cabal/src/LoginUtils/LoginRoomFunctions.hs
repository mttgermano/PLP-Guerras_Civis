{-# LANGUAGE DeriveGeneric #-}
module LoginUtils.LoginRoomFunctions where
import Core.DbFunctions

import GHC.Generics
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))



-- Room Data Type 
data Room = Room{ 
    rId :: String,
    rName :: String,
    rPassword :: String,
    rMaster :: String,
    isUp :: Bool,
    cursedWord :: Maybe String,
    roundMessages :: Maybe String
}deriving (Show, Generic)

-- Convert Room into a tuple for SQL insert
instance ToRow Room where
    toRow room = [ 
        toField (rId room), 
        toField (rName room), 
        toField (rPassword room), 
        toField (rMaster room), 
        toField (isUp room), 
        toField (cursedWord room),
        toField (roundMessages room)]

-- Create a room in the database
data CreateRoomResult = RoomCreated | RoomAlreadyExist String
createRoom :: String -> String -> String -> IO CreateRoomResult
createRoom player_name room_name room_password = do
    conn <- getDbConnection

    alreadyExist  <- checkRoomExist room_name 

    if alreadyExist
        then do
            let errMsg = "> Room name already exists"
            putStrLn errMsg
            return (RoomAlreadyExist errMsg)

        else do
            uuid <- fmap toString nextRandom    -- Generate random UUID

            let newRoom = Room { 
                rId = uuid, 
                rName = room_name,
                rPassword = room_password, 
                rMaster = player_name,
                isUp = False,
                cursedWord = Nothing,
                roundMessages = Nothing
            }

            -- DB Query ----------------------------------
            let sqlQuery1 = Query $ BS2.pack "INSERT INTO Room (room_uuid, room_name, room_password, room_master, is_up, cursed_word, round_messages) VALUES (?, ?, ?, ?, ?, ?, ?)"
            _ <- execute conn sqlQuery1 newRoom 
            ----------------------------------------------
            close conn
            putStrLn $ ("> Room created: " ++ show newRoom)

            -- auto Login the RoomMaster
            loginRoom player_name room_name room_password

            return RoomCreated

-- Chek if a room already exist in the database
checkRoomExist :: String -> IO Bool
checkRoomExist room_name = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT EXISTS (SELECT 1 FROM Room WHERE room_name = ?)"
    [Only result] <- query conn sqlQuery (Only room_name)
    ----------------------------------------------
    close conn
    return result

-- Log in a room
data LoginRoomResult = RoomLoggedIn | IncorrectRoomData String
loginRoom :: String -> String -> String -> IO LoginRoomResult
loginRoom player_name room_name room_password = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT room_uuid FROM Room WHERE room_name = ? AND room_password = ?"
    result <- query conn sqlQuery (room_name, room_password) :: IO [Only String]
    ----------------------------------------------

    -- Check if the query returned any rows
    if null result
        then do
            let errMsg = "> Invalid room name or password."
            putStrLn errMsg
            return (IncorrectRoomData errMsg)

        else do
            let room_uuid = fromOnly (head result)

            -- DB Query change current room --------------
            let sqlQuery1 = Query $ BS2.pack "UPDATE Player SET current_room = ? WHERE player_name = ?"
            _ <- execute conn sqlQuery1 (room_uuid, player_name)
            ----------------------------------------------
            
            -- DB  Query update GameRoomData -------------
            let sqlQuery2 = Query $ BS2.pack "UPDATE Player SET current_room = ? WHERE player_name = ?"
            _ <- execute conn sqlQuery2 (room_uuid, player_name)
            ----------------------------------------------
            close conn

            putStrLn ("> Player [" ++ player_name ++ "] login successful in [" ++ room_name ++ "]")
            return RoomLoggedIn
