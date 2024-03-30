module Game.RoundFunctions where

import Core.DbFunctions
import Game.GameFunctions
import Game.StartFunctions
import Utils.Utils

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))
import Control.Monad (forM_)



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
    putStrLn $ ("> [" ++ (rName) ++ "] Room - Começando Round Mafiosos")
    updateRoundState rName "evilRound" 

    evilList <- getRoomPlayersGoodness rName False

    --sleep 1

    -- send a request to the front, so it will allert that the user can make an action
    -- deixa rodar por 2 min
    -- desfaz a acao

    putStrLn $ ("> [" ++ (rName) ++ "] Room - Terminou Round Mafiosos")


-- Run the good guys round
actionGoodRound :: String -> IO ()
actionGoodRound rName = do
    putStrLn $ ("> [" ++ (rName) ++ "] Room - Começando Round Civis")
    updateRoundState rName "goodRound" 

    goodList <- getRoomPlayersGoodness rName True

    --sleep 1

    putStrLn $ ("> [" ++ (rName) ++ "] Room - Terminou Round Civis")


-- Reset all room players atributes
resetRoomPlayersAtributes :: String -> IO ()
resetRoomPlayersAtributes rName = do
    conn    <- getDbConnection
    pList   <- getRoomPlayersUUIDList rName
    let atributes = ["votes", "kill_vote", "is_paralized", "is_silenced"]

    -- For each pUUID and attribute, execute the SQL query
    forM_ pList $ \pUUID ->
        forM_ atributes $ \atribute -> do
            let sqlQuery = Query $ BS2.pack $ "UPDATE UserGameData SET " ++ atribute ++ " = 0 WHERE player_uuid = ?"
            _ <- execute conn sqlQuery (Only pUUID)
            return ()
    close conn

