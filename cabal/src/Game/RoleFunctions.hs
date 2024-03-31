module Game.RoleFunctions where

import Core.DbFunctions
import Utils.Utils


import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))


errPermissionMessage :: String -> IO ()
errPermissionMessage pName = putStrLn $ ("> [" ++ (pName)  ++ "] User - doesn't have permission to execute the action")

-- Kill a player 
kill :: String -> String -> IO ()     
kill agent action_reciever = do
    allowed <- isAllowed agent
    
    if allowed
        then do
            conn    <- getDbConnection
            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET kill_vote = kill_vote + 1 WHERE player_uuid = ?"
            _ <- execute conn sqlQuery (Only agent)
            ----------------------------------------------
            putStrLn $ ("> [" ++ agent ++ "] User - Kill Vote for [" ++ action_reciever ++ "]")
            close conn

            role <- getRole action_reciever
            if role == 1
                then fbiIsWatching action_reciever agent
                else return ()
        else
            errPermissionMessage agent


-- aprentice logic
aprentice :: String -> String -> IO ()     
aprentice agent action_reciever = do
    allowed         <- isAllowed agent
    rName           <- getPlayerRoomName agent
    pList           <- getRoomPlayersUUIDList rName
    isAssassinAlive <- isRoleAlive pList 1

    if allowed && isAssassinAlive
        then do
            kill agent action_reciever

            role <- getRole action_reciever
            if role == 1
                then fbiIsWatching action_reciever agent
                else return ()
        else
            errPermissionMessage agent


-- aprentice logic
police :: String -> String -> IO ()     
police agent action_reciever = do
    allowed         <- isAllowed agent
    rName           <- getPlayerRoomName agent
    pList           <- getRoomPlayersUUIDList rName
    isJudgeAlive    <- isRoleAlive pList 8


    if allowed && not isJudgeAlive
        then do
            kill agent action_reciever
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
            let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET kill_vote = kill_vote -1 WHERE player_uuid = ?"
            result <- execute conn sqlQuery (Only agent)
            ----------------------------------------------
            putStrLn $ ("> [" ++ agent ++ "] User - Saved [" ++ (action_reciever) ++ "]")
            close conn

            role <- getRole action_reciever
            if role == 1
                then fbiIsWatching action_reciever agent
                else return ()
        else
            errPermissionMessage agent


-- See the identity of a player
search :: String -> String -> IO ()
search agent action_reciever = do
    allowed <- isAllowed agent

    -- TODO
    if allowed
        then do
            revealPlayerRole agent action_reciever
            role <- getRole action_reciever

            if role == 1
                then fbiIsWatching action_reciever agent
                else return ()
        else
            errPermissionMessage agent


-- Reveal the indetity of a player to all the other ones
reveal :: String -> String -> IO ()
reveal agent action_reciever = do
    allowed <- isAllowed agent
    rName   <- getPlayerRoomName agent
    pList   <- getRoomPlayersUUIDList rName

    -- TODO
    if allowed
        then do
            revealToAll pList action_reciever
            role <- getRole action_reciever

            if role == 1
                then fbiIsWatching action_reciever agent
                else return ()
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
            let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET is_silenced = 1 WHERE player_uuid = ?"
            result <- execute conn sqlQuery (Only agent)
            ----------------------------------------------
            putStrLn $ ("> [" ++ agent ++ "] User - Silenced [" ++ (action_reciever) ++ "]")
            close conn

            role <- getRole action_reciever
            if role == 1
                then fbiIsWatching action_reciever agent
                else return ()
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
            let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET is_paralized = 1 WHERE player_uuid = ?"
            result <- execute conn sqlQuery (Only agent)
            ----------------------------------------------
            putStrLn $ ("> [" ++ agent ++ "] User - Paralized [" ++ (action_reciever) ++ "]")
            close conn

            role <- getRole action_reciever
            if role == 1
                then fbiIsWatching action_reciever agent
                else return ()
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
            putStrLn $ ("> [" ++ agent ++ "] User - setted Cursed Word for [" ++ "] rName")
        else
            errPermissionMessage agent

-- The revenge of a spirit
revenge :: String -> String -> IO ()
revenge agent action_reciever = do
    conn    <- getDbConnection
    allowed <- isAllowed agent

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT kill_vote FROM UserGameData WHERE player_uuid = ?"
    result <- execute conn sqlQuery (Only agent)
    ----------------------------------------------

    if allowed && result > 0
        then do
            kill agent action_reciever
        else
            errPermissionMessage agent

-- Reveal who took action in the police.
fbiIsWatching :: String -> String -> IO()
fbiIsWatching police actionMaker = do
    revealPlayerRole police actionMaker


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
