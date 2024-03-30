module Game.RoundFunctions where

import Core.DbFunctions
import Game.GameFunctions
import Game.StartFunctions
import Utils.Utils

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))




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

    sleep 1

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

    sleep 1

    putStrLn $ ("> Terminou Round Civis [" ++ (rName) ++ "]")
