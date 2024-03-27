module GameUtils.RoleFunctions where

import Core.DbFunctions
import GameUtils.RoundFunctions

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))


errPermissionMessage :: String -> IO ()
errPermissionMessage pName = putStrLn $ ("> User [" ++ pName ++ "] doesn't permission to execute the action")

-- Kill a player 
kill :: String -> String -> IO ()     
kill agent action_reciever = do
    allowed <- isAllowed agent

    if allowed
        then do
            conn    <- getDbConnection

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET kill_vote = true WHERE user_id = ?"
            result <- execute conn sqlQuery (Only agent)
            ----------------------------------------------
            putStrLn $ ("> User [" ++ agent ++ "] Kill Vote for [" ++ (action_reciever) ++ "]")
            close conn
        else
            errPermissionMessage agent
    

-- Save a player of being killed
save :: String -> String -> IO ()
save agent action_reciever = do
    allowed <- isAllowed agent

    if allowed
        then do
            conn    <- getDbConnection
    
            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET kill_vote = kill_vote -1 WHERE user_id = ?"
            result <- execute conn sqlQuery (Only agent)
            ----------------------------------------------
            putStrLn $ ("> User [" ++ agent ++ "] Saved [" ++ (action_reciever) ++ "]")
            close conn
        else
            errPermissionMessage agent


-- See the identity of a player
search :: String -> String -> IO ()
search agent action_reciever = do
    allowed <- isAllowed agent

    -- TODO
    if allowed
        then do
            conn    <- getDbConnection
            putStrLn $ ("> User [" ++ agent ++ "] Searched Role for [" ++ (action_reciever) ++ "]")
        else
            errPermissionMessage agent

-- Reveal the indetity of a player to all the other ones
reveal :: String -> String -> IO ()
reveal agent action_reciever = do
    allowed <- isAllowed agent

    -- TODO
    if allowed
        then do
            conn    <- getDbConnection
            putStrLn $ ("> User [" ++ agent ++ "] Revealed [" ++ (action_reciever) ++ "]")
        else
            errPermissionMessage agent


-- Silence a player
silence :: String -> String -> IO ()
silence agent action_reciever = do
    allowed <- isAllowed agent

    if allowed
        then do
            conn    <- getDbConnection

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET is_silenced = true WHERE user_id = ?"
            result <- execute conn sqlQuery (Only agent)
            ----------------------------------------------
            putStrLn $ ("> User [" ++ agent ++ "] Silenced [" ++ (action_reciever) ++ "]")
            close conn
        else
            errPermissionMessage agent


-- Paralize a player
paralize :: String -> String -> IO ()
paralize agent action_reciever = do
    allowed <- isAllowed agent

    if allowed
        then do
            conn    <- getDbConnection

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET is_paralized = true WHERE user_id = ?"
            result <- execute conn sqlQuery (Only agent)
            ----------------------------------------------
            putStrLn $ ("> User [" ++ agent ++ "] Paralized [" ++ (action_reciever) ++ "]")
            close conn
        else
            errPermissionMessage agent


-- Set the Cursed word for a Room
setCursedWord :: String -> String -> IO ()
setCursedWord agent cursedWord = do
    allowed <- isAllowed agent

    if allowed
        then do
            conn    <- getDbConnection

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "UPDATE Room SET cursed_word = ? WHERE room_uuid = (SELECT current_room FROM Player WHERE player_name = ?)"
            result <- execute conn sqlQuery (cursedWord, agent)
            ----------------------------------------------
            putStrLn $ ("> User [" ++ agent ++ "] set Cursed Word for [" ++ "] rName")
        else
            errPermissionMessage agent
