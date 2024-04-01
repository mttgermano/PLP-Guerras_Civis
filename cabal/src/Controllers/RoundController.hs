module Controllers.RoundController where

import GHC.Generics (Constructor (conFixity))
import Game.BotLogic
import Game.ChatFunctions
import Game.GameFunctions
import Game.RoundFunctions
import Game.StartFunctions
import Game.StopFunctions
import Utils.Utils

-- Run the Round which the users can vote
voteRound :: String -> IO ()
voteRound rName = do
    putStrLn $ ("> [" ++ (rName) ++ "] Room - Começando Vote   Round ")

    updateRoundState rName "vote"

    putStrLn $ replicate 50 '-'

-- The Round where players can execute an action
actionRound :: String -> IO ()
actionRound rName = do
    putStrLn $ ("> [" ++ (rName) ++ "] Room - Começando Action Round ")
    updateRoundState rName "action"
    sleep 1



checkEndGame :: String -> Int -> IO ()
checkEndGame rName roundNum = do
    cTeamEvil <- getPlayerRolesCount rName False
    cTeamGood <- getPlayerRolesCount rName True

    if cTeamGood == 0 
        then do 
            endGame rName "evilWins"
        else if cTeamEvil == 0
            then do
                endGame rName "goodWins"
            else if roundNum == 5   
                then do
                    endGame rName "roundLimit"
                else
                    return () 

-- Finish the game
endGame :: String -> String -> IO ()
endGame rName reason = do
    putStrLn $ ("> [" ++ (rName) ++ "] Room - o jogo  acabou!" ++ reason)
    updateRoundState rName reason
    deleteUserGameData          rName
    deleteRoomPlayersKnowledge  rName
    deleteRoomBots              rName
    resetPlayersCurrentRoom     rName
    deleteRoom                  rName
