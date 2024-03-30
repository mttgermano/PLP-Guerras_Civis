module Controllers.RoundController where

import Game.RoundFunctions
import Game.StartFunctions
import Game.GameFunctions
import Utils.Utils

-- Run the Round that the actions happend
actionRound :: String -> IO ()
actionRound rName = do
    actionEvilRound rName
    actionGoodRound rName

-- Run the Round which the users can vote
voteRound :: String -> IO ()
voteRound rName = do
    putStrLn $ ("> Começando Round da Votação  [" ++ (rName) ++ "]")
    updateRoundState rName "voteRound"

    sleep 5

    putStrLn $ ("> Término Round da Votação    [" ++ (rName) ++ "]")
    putStrLn $ replicate 50 '-'

-- Run all the sequence of rounds
runRound :: String -> IO ()
runRound rName = do
    actionRound             rName
    voteRound               rName
    roundDefaultSettings    rName

-- Reset all the setting of a Room
roundDefaultSettings :: String -> IO ()
roundDefaultSettings rName = do
    updateRoundState    rName "startRound"
    resetRoundMessages  rName   

