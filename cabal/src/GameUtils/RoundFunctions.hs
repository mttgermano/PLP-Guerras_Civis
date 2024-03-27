module GameUtils.RoundFunctions where

import Core.DbFunctions
import GameUtils.GameFunctions
import GameUtils.GameStartFunctions

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))



-- It selects the itens of the first list whom index, in the second list is equal to flag
selectTheIndex :: [String] -> [Bool] -> Bool -> [String]
selectTheIndex pList gList b = [x | (x, flag) <- zip pList gList, flag == b]

-- Get the list of all good players in a room
getRoomPlayersGoodness :: String -> Bool -> IO [String]
getRoomPlayersGoodness rName isGood = do
    rPlayers        <- getRoomPlayers rName
    goodnessList    <- mapM getIsGood rPlayers

    return $ selectTheIndex rPlayers goodnessList isGood

-- Get the list of all good bots in a room
getRoomBotsGoodness :: String -> Bool -> IO [String]
getRoomBotsGoodness rName isGood = do
    rBots        <- getRoomBots rName
    goodnessList    <- mapM getIsGood rBots

    return $ selectTheIndex rBots goodnessList isGood

-- Update the round state of the Room
updateRoundState :: String -> String -> IO ()
updateRoundState room_name state = do
    conn <- getDbConnection

    -- DB Query change current room --------------
    let sqlQuery = Query $ BS2.pack "UPDATE Room SET round_state = ? WHERE room_name = ?"
    _ <- execute conn sqlQuery (state, room_name)
    ----------------------------------------------
    close conn

-- Run the evil guys round
actionEvilRound :: String -> IO ()
actionEvilRound rName = do
    putStrLn $ ("> Começando Round Mafiosos [" ++ (rName) ++ "]")
    updateRoundState rName "evilRound" 

    evilList <- getRoomPlayersGoodness rName False

    sleep 5

    -- send a request to the front, so it will allert that the user can make an action
    -- deixa rodar por 2 min
    -- desfaz a acao

    putStrLn $ ("> Terminou Round Mafiosos [" ++ (rName) ++ "]")


-- Run the good guys round
actionGoodRound :: String -> IO ()
actionGoodRound rName = do
    putStrLn $ ("> Começando Round Civis [" ++ (rName) ++ "]")
    updateRoundState rName "goodRound" 

    goodList <- getRoomPlayersGoodness rName True

    sleep 5

    putStrLn $ ("> Terminou Round Civis [" ++ (rName) ++ "]")


-- Get player name from id
getPlayerFromID :: String -> IO String
getPlayerFromID pUUID = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT player_name FROM Player WHERE player_uuid = ?"
    [Only pName] <- query conn sqlQuery [pUUID]
    ----------------------------------------------
    close conn
    return pName

-- Get player ID from Name
getUUIDFromPlayerName :: String -> IO String
getUUIDFromPlayerName pName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT player_uuid FROM Player WHERE player_name = ?"
    [Only pUUID] <- query conn sqlQuery [pName]
    ----------------------------------------------
    close conn
    return pUUID


-- Return a list with the players names
getPlayersNames :: [String] -> IO [String]
getPlayersNames [] = return []
getPlayersNames (id:ids) = do
    pName           <- getPlayerFromID (show id)
    remainingStr    <- getPlayersNames ids

    return (pName : remainingStr)


-- Check if the player can do the action
isAllowed :: String ->  IO Bool
isAllowed pName = do
    conn    <- getDbConnection

    pUUID   <- getUUIDFromPlayerName pName
    pRoom   <- getPlayerRoomName pUUID
    
    pRoleIsGood     <- getIsGood pUUID
    rState          <- getRoomRoundState pRoom

    return $ (not pRoleIsGood && rState == "evilRound") || (pRoleIsGood && rState == "goodRound")
