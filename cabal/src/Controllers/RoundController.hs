module Controllers.RoundController where

import GHC.Generics (Constructor (conFixity))
import Game.BotLogic
import Game.ChatFunctions
import Game.GameFunctions
import Game.Interface
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

<<<<<<< HEAD
-- Run all the sequence of rounds
runRound :: String -> IO ()
runRound rName = do
  updateRoundState rName "action"
  drawInterface
  -- roundDefaultSettings rName
  actionRound rName
  botsRound rName
  -- getRoomActionsResults   rName
  roundResult rName -- --> se X ta com kill_vote 1, print: User morreu y, coloca is_alive pra 0, ...
  voteRound rName
  --sendMessage "john" "o culpado e john"
  voteBotsRound rName
  computeVote
  clearRound rName
=======
-- The Round where players can execute an action
actionRound :: String -> IO ()
actionRound rName = do
    updateRoundState rName "action"
    putStrLn $ ("> [" ++ (rName) ++ "] Room - Começando Action Round ")
    sleep 1
    putStrLn $ ("> [" ++ (rName) ++ "] Room - Acabando Action Round ")
>>>>>>> b4f6f7a33d03b135835472d7fd74ec28b08b78d9



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
