module GameController where

import GameFunctions
import GameFunctionsInit
import GameRoundController



-- Game Logic
game :: String -> Int -> Int -> Int -> IO ()
game rName roundNum teamEvil teamGood
    | teamGood == 0         = endGame rName "evilWins"
    | teamEvil == 0         = endGame rName "goodWins"
    | roundNum == 5         = endGame rName "roundLimit"
    | otherwise = do
        makeRound rName
        -- DB Query ----------------------------------
        cTeamEvil <- getPlayerRolesCount False rName
        cTeamGood <- getPlayerRolesCount True rName
        ----------------------------------------------
        -- make request to the front, saying that need to fresh the data
        game rName (roundNum + 1) cTeamEvil cTeamGood

-- Start the game
startGame :: String -> String -> IO ()
startGame rName pName = do
    roomMaster <- isRoomMaster rName pName

    if roomMaster
        then do
            putStrLn $ "The " ++ rName ++ " game started!"
            distributeRoles rName
            game rName 0 6 6
        else putStrLn "You are not the room master."


-- Finish the game
-- TODO
endGame :: String -> String -> IO ()
endGame rName reason = do
    putStrLn $ "The " ++ (rName) ++ " game finished!"
    -- make post request to the front end, saying that it need to go to the endGame page

-- Make the Game Rounds
makeRound :: String -> IO ()
makeRound rName = runRound rName
