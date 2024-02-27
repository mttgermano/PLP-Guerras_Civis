{-# LANGUAGE DeriveGeneric #-}

module RoomFunctions where

import GHC.Generics
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BS1
import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))


-- Room data type 
data Room = Room
    { rId :: String,
      rName :: String,
      rPassword :: String
    }
    deriving (Show, Generic)

-- Automatically derive FromJSON instance
instance FromJSON Room

-- Convert Room into a tuple for SQL insert
instance ToRow Room where
    toRow room = [toField (rId room), toField (rName room), toField (rPassword room)]


-- Create a room in the database
createRoom :: BS1.ByteString -> IO ()
createRoom jsonRequest = do
    let maybeRoom = parseRoomRequest jsonRequest

    case maybeRoom of
        Just room -> do
            uuid <- fmap toString nextRandom    -- Generate random UUID
            conn <- getDbConnection

            let newRoom = room { rId = uuid }

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "INSERT INTO rooms (room_id, room_name, room_password) VALUES (?, ?, ?)"
            _ <- execute conn sqlQuery newRoom 
            ----------------------------------------------
            close conn
            putStrLn $ "Room created: " ++ show newRoom 

        Nothing ->
            putStrLn "Invalid JSON format."


-- Log in a room
login :: BS1.ByteString -> IO ()
login jsonRequest = do
    let maybeRoom = parseRoomRequest jsonRequest

    case maybeRoom of
        Just room -> do
            conn <- getDbConnection

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "SELECT room_id FROM rooms WHERE room_name = ? AND room_password = ?"
            result <- query conn sqlQuery (roomName, roomPassword) :: IO [Only String]
            ----------------------------------------------
            close conn

            -- Check if the query returned any rows
            if null result
                then putStrLn "Invalid room name or password."
                else putStrLn "Login successful."

        Nothing ->
            putStrLn "Invalid JSON format."


-- Parse a POST request
parseRoomRequest :: BS1.ByteString -> Maybe Room
parseRoomRequest = decode

-- Establish a database connection
getDbConnection :: IO Connection
getDbConnection = connectPostgreSQL $ BS2.pack "host=localhost dbname=mydatabase user=myuser password=mypassword"
