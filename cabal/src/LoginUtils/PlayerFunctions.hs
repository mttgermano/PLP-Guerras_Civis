{-# LANGUAGE DeriveGeneric #-}
module LoginUtils.PlayerFunctions where
import Core.DbFunctions

import GHC.Generics
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)



-- Player Data Type
data Player = Player{ 
    isBot       :: Bool,
    pId         :: String,
    pName       :: String,
    pPassword   :: String,
    currentRoom :: Maybe String     -- Represents the presence of the user in a room
} deriving (Show, Generic)

-- Convert Player into a tuple for SQL insert
instance ToRow Player where
    toRow player = [toField (isBot player),toField (pId player), toField (pName player), toField (pPassword player), toField (currentRoom player)]

-- Instance for converting SQL row to Player datatype
instance FromRow Player where
    fromRow = Player <$> field <*> field <*> field <*> field <*> field

-- Create a player in the db
data CreatePlayerResult = PlayerCreated [Player] | PlayerAlreadyExist String
createPlayer :: String -> String -> IO CreatePlayerResult
createPlayer player_name player_password = do
    conn <- getDbConnection
    alreadyExist  <- checkPlayerExist player_name

    if alreadyExist
        then do
            let errMsg = "> Player name already exists"
            putStrLn errMsg
            return (PlayerAlreadyExist errMsg)

        else do
            uuid <- fmap toString nextRandom    -- Generate random UUID

            let newPlayer = Player { isBot = False, pId = uuid, pName = player_name, pPassword = player_password, currentRoom = Nothing}

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "INSERT INTO Player (is_bot ,player_uuid, player_name, player_password, current_room) VALUES (?, ?, ?, ?, ?)"
            _ <- execute conn sqlQuery newPlayer
            ----------------------------------------------
            close conn

            putStrLn $ ("> Player created [" ++ show newPlayer ++ "]")
            return (PlayerCreated [newPlayer])

-- Chek if a player already exist in the database
checkPlayerExist :: String -> IO Bool
checkPlayerExist player_name = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT EXISTS (SELECT 1 FROM Player WHERE player_name = ?)"
    [Only result] <- query conn sqlQuery (Only player_name)
    ----------------------------------------------
    close conn
    return result

-- Log in a player
data LoginPlayerResult = PlayerLoggedIn [Player] | IncorrectPlayerData String
loginPlayer :: String -> String -> IO LoginPlayerResult
loginPlayer player_name player_password = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery1 = Query $ BS2.pack "SELECT player_uuid FROM Player WHERE player_name = ? AND player_password = ?"
    result <- query conn sqlQuery1 (player_name, player_password) :: IO [Only String]
    ----------------------------------------------

    -- Check if the query returned any rows
    if null result
        then do
            let errMsg = "> Invalid playername or password."
            putStrLn errMsg
            close conn
            return (IncorrectPlayerData errMsg)
        else do 
            putStrLn ("> Login [" ++ player_name ++"] successful.")

            -- DB Query ----------------------------------
            let sqlQuery2 = Query $ BS2.pack "SELECT is_bot, player_uuid, player_name, player_password, current_room FROM Player WHERE player_name = ?"
            results <- query conn sqlQuery2 (Only player_name) :: IO [Player]
            ----------------------------------------------
            close conn

            return (PlayerLoggedIn results)
