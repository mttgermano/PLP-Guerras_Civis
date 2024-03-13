module GameRoundController where
import GameRoundFunctions

actionRound :: String -> IO ()
actionRound rName = do
    actionEvilRound rName
    actionGoodRound rName

voteRound :: String -> IO ()
voteRound rName = do
    putStrLn $ "Começando Round da Votação no " ++ (rName)
    -- todo 
    putStrLn $ "Término Round da Votação no " ++ (rName)

runRound :: String -> IO ()
runRound rName = do
    actionRound rName
    voteRound rName
