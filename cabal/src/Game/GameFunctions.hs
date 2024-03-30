module Game.GameFunctions where

import Core.DbFunctions
import Game.StartFunctions
import Utils.Utils

import qualified Data.ByteString.Char8 as BS2
import Control.Concurrent (threadDelay)
import GHC.Generics

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))
import GHC.Base (IO)



-- User Data Type
data UserGame = UserGame { 
    pId_ :: String
} deriving (Show, Generic)

-- Convert User into a tuple for SQL insert
instance ToRow UserGame where
    toRow userGame = [toField (pId_ userGame)]


sleep :: Int -> IO ()
sleep minutes = do
    let delay = minutes * (60 * 1000000)
    threadDelay delay

-- Increment the vote for a user in the db
incrementVote :: String -> String -> IO ()   
incrementVote pName pName_voted = do
    conn    <- getDbConnection
    pUUID   <- getUUIDFromPlayerName pName_voted

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET votes = votes + 1 WHERE player_uuid = ?"
    _ <- execute conn sqlQuery (Only pUUID)
    ----------------------------------------------
    putStrLn $ ("> Voto incrementado para user [" ++ (pName_voted) ++ "]")
    close conn


-- Get a Bool telling if the player is alive - using its UUID
isPlayerAlive ::  String -> IO Bool
isPlayerAlive pUUID = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT is_alive FROM UserGameData WHERE player_uuid = ?"    
    result <- query conn sqlQuery (Only pUUID)
    ----------------------------------------------
    close conn
    case result of
        [Only alive] -> return alive
        _            -> return False

-- Check if the player can do the action
isAllowed :: String ->  IO Bool
isAllowed pName = do
    pUUID   <- getUUIDFromPlayerName pName
    pRoom   <- getPlayerRoomName pUUID
    rState  <- getRoomRoundState pRoom

    return $ (rState == "actionRound")


isRoleAlive :: [String] -> Int -> IO Bool
isRoleAlive [] _ = return False
isRoleAlive (playerId:rest) role = do
    isAlive <- isPlayerAlive playerId
    playerRole <- getRole playerId
    if isAlive && playerRole == role
        then return True
        else isRoleAlive rest role


revealPlayerRole :: String -> String -> IO ()
revealPlayerRole agent agent_reciever = do
    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "INSERT INTO RoleKnowledge (who_knows, who_is_known) VALUES (?, ?)"
    _ <- execute conn sqlQuery (agent, agent_reciever)
    ----------------------------------------------
    close conn


revealToAll :: [String] -> String -> IO ()
revealToAll players action_receiver = mapM_ (\id -> revealPlayerRole id action_receiver) players
