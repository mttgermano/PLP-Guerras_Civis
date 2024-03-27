{-# LANGUAGE DeriveGeneric #-}
module GameUtils.GameFunctionsInit where
import Core.DbFunctions

import System.Random
import Data.List (permutations)
import Control.Monad (zipWithM_)
import GHC.Generics (Generic)  

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.Types (Query(Query))
import Database.PostgreSQL.Simple.ToField (toField)



-- UserGame Data Type 
data UserGameData = UserGameData{
    player_uuid :: String, 
    role_idx    :: Int, 
    is_alive    :: Bool, 
    votes       :: Int, 
    kill_vote   :: Int, 
    is_paralized    :: Bool, 
    is_silenced     :: Bool, 
    is_dead_by_cursed_word :: Bool
}deriving (Show, Generic)

-- Convert UserGameData into a tuple for SQL insert
instance ToRow UserGameData where
    toRow userGameData = [ 
        toField (player_uuid    userGameData), 
        toField (role_idx       userGameData), 
        toField (is_alive       userGameData), 
        toField (votes          userGameData), 
        toField (kill_vote      userGameData), 
        toField (is_paralized   userGameData),
        toField (is_silenced    userGameData),
        toField (is_dead_by_cursed_word userGameData)]


-- Build up a Room
setRoomUpState :: String -> Bool -> IO ()
setRoomUpState rName state = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE Room SET is_up = ? WHERE room_name = ?"
    _ <- execute conn sqlQuery (state, rName)
    ----------------------------------------------
    close conn


-- Set the Role of a Player
setRole :: String -> String -> IO ()
setRole pName role = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET role_idx = ? WHERE player_uuid = ?"
    _ <- execute conn sqlQuery (role, pName)
    ----------------------------------------------
    putStrLn $ ("> User [" ++ (pName) ++ "] foi settado para [" ++ (role) ++ "]")
    close conn


-- Get all Players UUID from a Room
getRoomPlayers :: String -> IO [String]
getRoomPlayers rName = do
    conn    <- getDbConnection
    rUuid   <- getRoomUuid rName

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT player_uuid FROM Player WHERE current_room = ?"    
    result <- query conn sqlQuery (Only rUuid)
    ----------------------------------------------
    close conn

    let pList = (map (\(Only player_uuid) -> player_uuid) result)
    return pList


-- Random List of Integers
randomList :: Int -> IO [Int]
randomList n = do
    gen <- newStdGen
    let nums = take n $ permutations [1..n]
        (index, _) = randomR (0, length nums - 1) gen
    return $ nums !! index


addPlayersToGame :: String -> IO ()
addPlayersToGame rName = do
    conn            <- getDbConnection
    roomPlayers     <- getRoomPlayers rName

    -- Iterate over roomPlayers and perform insertion for each player
    mapM_ (\playerUuid -> do
        let newUserGameData = UserGameData {
            player_uuid     = playerUuid,
            role_idx        = (-1),
            is_alive        = True,
            votes           = 0,
            kill_vote       = 0,
            is_paralized    = False,
            is_silenced     = False,
            is_dead_by_cursed_word = False
        }

        -- DB Query ----------------------------------
        let sqlQuery = Query $ BS2.pack "INSERT INTO UserGameData (player_uuid, role_idx, is_alive, votes, kill_vote, is_paralized, is_silenced, is_dead_by_cursed_word) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        execute conn sqlQuery (toRow newUserGameData)
        ----------------------------------------------
        ) roomPlayers
    close conn

    putStrLn $ ("> All players from [" ++ rName ++ "] are in the game")


-- Distribute Roles to the players
distributeRoles :: String -> IO ()
distributeRoles rName = do 
    let action = "[Roles distribution] [" ++ (rName) ++ "]" ++ replicate 10 '-'
    putStrLn  action
    
    roles_index  <- randomList 12
    room_players <- getRoomPlayers rName

    zipWithM_ (\player roleIndex -> setRole player (show roleIndex)) room_players roles_index

    putStrLn $ replicate (length action) '-'


-- Check if the player is the room master
isRoomMaster :: String -> String -> IO Bool
isRoomMaster rName pName = do
    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT room_master FROM Room WHERE room_name = ?"
    [Only result] <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
    return $ result == pName


getRoomBots :: String -> IO [String]
getRoomBots rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "\
               \SELECT u.player_uuid \
               \FROM Player p \
               \JOIN UserGameData u ON p.player_uuid = u.player_uuid \
               \WHERE p.current_room = ? AND p.is_bot = true;"    
    result <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
    return $ map (\(Only player_uuid) -> player_uuid) result

-- Get the UUID of a Room
getRoomUuid :: String -> IO String
getRoomUuid rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT room_uuid FROM Room WHERE room_name = ?"    
    [Only result] <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn

    return result

-- Get the State of a Room
getRoomUpState :: String -> IO Bool
getRoomUpState rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT is_up FROM Room WHERE room_name = ?"    
    [Only result] <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
    return result

getRoomRoundState :: String -> IO String
getRoomRoundState rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT round_state FROM Room WHERE room_name = ?"    
    [Only state] <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
    return state

-- Reset the Round Messages
resetRoundMessages :: String -> IO ()
resetRoundMessages rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE Room SET round_messages = '' WHERE room_name = ?"
    _ <- execute conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
