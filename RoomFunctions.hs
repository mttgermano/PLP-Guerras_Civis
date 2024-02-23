module RoomFunctions where

import Control.Exception (bracket)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

-- Room data type 
data Room = Room
    { roomId :: String,
      roomName :: String,
      roomPassword :: String
    }
    deriving (Show)

-- Convert Room into a tuple for SQL insert
instance ToRow Room where
    toRow room = [toField (roomId room), toField (roomName room), toField (roomPassword room)]

-- Create a room in the database
createRoom :: Room -> IO ()
createRoom room = do
    uuid <- fmap toString nextRandom    -- Generate random UUID
    conn <- getDbConnection

    -- DB Query ----------------------------------
    execute conn "INSERT INTO rooms (room_id, room_name, room_password) VALUES (?, ?, ?)" room
    ----------------------------------------------
    close conn


-- Parse a POST request
parseRoomRequest :: BS.ByteString -> Maybe Room
parseRoom
