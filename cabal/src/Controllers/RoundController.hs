module Controllers.RoundController where

import Game.RoundFunctions
import Game.StartFunctions
import Game.GameFunctions
import Game.BotLogic
import Utils.Utils
import GHC.Generics (Constructor(conFixity))



-- Run the Round which the users can vote
voteRound :: String -> IO ()
voteRound rName = do
    putStrLn $ ("> [" ++ (rName) ++ "] Room - Começando Vote   Round ")
    updateRoundState rName "voteRound"

    --sleep 5

    putStrLn $ ("> [" ++ (rName) ++ "] Room - Terminou  Vote   Round")
    putStrLn $ replicate 50 '-'

-- Run all the sequence of rounds
runRound :: String -> IO ()
runRound rName = do
    updateRoundState rName "actionRound"
    roundDefaultSettings    rName
    actionRound             rName
    botsRound               rName
    voteRound               rName
    voteBotsRound           rName
    roundResult             rName -- --> se X ta com kill_vote 1, print: User morreu y, coloca is_alive pra 0, ...

-- Reset all the setting of a Room
roundDefaultSettings :: String -> IO ()
roundDefaultSettings rName = do
    updateRoundState                rName "startRound"
    resetRoundMessages              rName   
    resetRoomPlayersAtributes       rName
