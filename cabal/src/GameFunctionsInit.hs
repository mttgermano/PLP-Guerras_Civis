module GameFunctionsInit where
import DbFunctions

import System.Random
import Data.List (permutations)
import Control.Monad (zipWithM_)

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))



-- Set the Role of a Player
setRole :: String -> String -> IO ()
setRole pName role = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET role = ? WHERE user_id = ?"
    result <- execute conn sqlQuery (role, pName)
    ----------------------------------------------
    putStrLn $ ("> User [" ++ (pName) ++ "] foi settado para [" ++ (role) ++ "]")
    close conn


-- Get all Players from a Room
getRoomPlayers :: String -> IO [String]
getRoomPlayers rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "\
               \SELECT u.player_uuid \
               \FROM Player p \
               \JOIN UserGameData u ON p.player_uuid = u.player_uuid \
               \WHERE p.current_room = ?;"    
    result <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
    return $ map (\(Only player_uuid) -> player_uuid) result


-- Random List of Integers
randomList :: Int -> IO [Int]
randomList n = do
    gen <- newStdGen
    let nums = take n $ permutations [1..n]
        (index, _) = randomR (0, length nums - 1) gen
    return $ nums !! index


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
    result <- query conn sqlQuery (Only pName)
    ----------------------------------------------
    close conn
    return (result == [pName])
