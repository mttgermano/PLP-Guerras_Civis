module GameRoundController where
import GameRoundFunctions


-- Run the Round that the actions happend
actionRound :: String -> IO ()
actionRound rName = do
    actionEvilRound rName
    actionGoodRound rName

-- Run the Round which the users can vote
voteRound :: String -> IO ()
voteRound rName = do
    putStrLn $ ("> Começando Round da Votação  [" ++ (rName) ++ "]")
    -- todo 
    putStrLn $ ("> Término Round da Votação  [" ++ (rName) ++ "]")
    putStrLn $ replicate 50 '-'

-- Run all the sequence of rounds
runRound :: String -> IO ()
runRound rName = do
    actionRound rName
    voteRound rName
