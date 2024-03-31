module Controllers.RoundController where

import Game.RoundFunctions
import Game.StartFunctions
import Game.GameFunctions
import Utils.Utils



-- Run the Round which the users can vote
voteRound :: String -> IO ()
voteRound rName = do
    putStrLn $ ("> [" ++ (rName) ++ "] Room - Começando Vote   Round ")
    updateRoundState rName "voteRound"

    --sleep 1

    putStrLn $ ("> [" ++ (rName) ++ "] Room - Terminou  Vote   Round")
    putStrLn $ replicate 50 '-'

-- Run all the sequence of rounds
runRound :: String -> IO ()
runRound rName = do
    roundDefaultSettings    rName
    actionRound             rName
    voteRound               rName
    -- roundResult          rName --> se X ta com kill_vote 1, print: User morreu y, coloca is_alive pra 0, ...

-- Reset all the setting of a Room
roundDefaultSettings :: String -> IO ()
roundDefaultSettings rName = do
    updateRoundState                rName "startRound"
    resetRoundMessages              rName   
    resetRoomPlayersAtributes       rName
