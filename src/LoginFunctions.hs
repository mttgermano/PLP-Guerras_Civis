{-# LANGUAGE DeriveGeneric #-}
--module LoginFunctions (createPlayer, loginPlayer) where
module LoginFunctions where
import DbFunctions

import GHC.Generics
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))



-- Player Data Type
data Player = Player
    { pId :: String,
      pName :: String,
      pPassword :: String,
      cRoom :: Maybe String     -- Represents the presence of the user in a room
    }
    deriving (Show, Generic)

-- Convert Player into a tuple for SQL insert
instance ToRow Player where
    toRow player = [toField (pId player), toField (pName player), toField (pPassword player), toField (cRoom player)]

-- Create a player in the db
createPlayer :: String -> String -> IO ()
createPlayer player_name player_password = do
    uuid <- fmap toString nextRandom    -- Generate random UUID
    conn <- getDbConnection

    let newPlayer = Player { pId = uuid, pName = player_name, pPassword = player_password, cRoom = Nothing}

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "INSERT INTO Player (player_uuid, player_name, player_password, current_room) VALUES (?, ?, ?)"
    result <- execute conn sqlQuery newPlayer
    print result
    ----------------------------------------------
    close conn
    putStrLn $ "Player created: " ++ show newPlayer


-- Log in a player
loginPlayer :: String -> String -> IO ()
loginPlayer player_name player_password = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT player_uuid FROM Player WHERE player_name = ? AND player_password = ?"
    result <- query conn sqlQuery (player_name, player_password) :: IO [Only String]
    ----------------------------------------------
    close conn

    -- Check if the query returned any rows
    if null result
        then putStrLn "Invalid playername or password."
        else putStrLn "Login successful."
