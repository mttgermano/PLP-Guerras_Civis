{-# LANGUAGE DeriveGeneric #-}
-- module RoomFunctions (createRoom, loginRoom) where
module RoomFunctions where
import DbFunctions

import GHC.Generics
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))



-- Room Data Type 
data Room = Room
    { rId :: String,
      rName :: String,
      rPassword :: String
    }
    deriving (Show, Generic)

-- Convert Room into a tuple for SQL insert
instance ToRow Room where
    toRow room = [toField (rId room), toField (rName room), toField (rPassword room)]

-- Create a room in the database
createRoom :: String -> String -> IO ()
createRoom room_name room_password = do
    uuid <- fmap toString nextRandom    -- Generate random UUID
    conn <- getDbConnection

    let newRoom = Room { rId = uuid, rName = room_name, rPassword = room_password}

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "INSERT INTO rooms (room_id, room_name, room_password) VALUES (?, ?, ?)"
    result <- execute conn sqlQuery newRoom 
    print result
    ----------------------------------------------
    close conn
    putStrLn $ "Room created: " ++ show newRoom 


-- Log in a room
loginRoom :: String -> String -> IO ()
loginRoom room_name room_password = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT room_id FROM rooms WHERE room_name = ? AND room_password = ?"
    result <- query conn sqlQuery (room_name, room_password) :: IO [Only String]
    ----------------------------------------------
    close conn

    -- Check if the query returned any rows
    if null result
        then putStrLn "Invalid room name or password."
        else putStrLn "Room login successful."
