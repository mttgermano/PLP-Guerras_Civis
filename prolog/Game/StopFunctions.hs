module Game.StopFunctions where
import Core.DbFunctions
import Game.StartFunctions
import Utils.Utils

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.Types (Query(Query))
import Database.PostgreSQL.Simple.ToField (toField)



-- Delete a Room in the database
deleteRoom :: String -> IO ()
deleteRoom rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "DELETE FROM Room WHERE room_name = ?"
    _ <- execute conn sqlQuery (Only rName)
    ----------------------------------------------
    putStrLn $ ("> [" ++ (rName) ++ "] Room - foi deletado")
    close conn


-- Delte all UserGameData entries for a Room - using its room_name
deleteUserGameData :: String -> IO ()
deleteUserGameData rName = do
    pList <- getRoomPlayersUUIDList rName
    mapM_ changeUserGameData pList
    putStrLn $ ("> [" ++ rName ++ "] Room - teve seu UserGameData deletado")


-- Delete the UserGameData entrie for a Player - using its UUID
changeUserGameData :: String -> IO ()
changeUserGameData pUUID = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "DELETE FROM UserGameData WHERE player_uuid = ?"
    _ <- execute conn sqlQuery (Only pUUID)
    ----------------------------------------------
    close conn


-- Remove the current_room of all Players that were playing - using the room name
resetPlayersCurrentRoom :: String -> IO ()
resetPlayersCurrentRoom rName = do
    pList <- getRoomPlayersUUIDList rName
    mapM_ changePlayerCurrentRoom pList


-- Change the current_room of a single Player - using its UUID
changePlayerCurrentRoom :: String -> IO ()
changePlayerCurrentRoom pUUID = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE Player SET current_room = NULL WHERE player_uuid = ?"
    _ <- execute conn sqlQuery (Only pUUID)
    ----------------------------------------------
    close conn
