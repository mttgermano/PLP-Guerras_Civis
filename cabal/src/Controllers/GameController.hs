module Controllers.GameController where

import Controllers.RoundController
import Game.StartFunctions
import Game.StopFunctions
import Game.GameFunctions
import Game.RoleFunctions
import Game.ChatFunctions
import Game.BotLogic
import Utils.Utils



-- Game Logic
game :: String -> Int -> Int -> Int -> IO ()
game rName roundNum teamEvil teamGood
    | teamGood == 0         = endGame rName "evilWins"
    | teamEvil == 0         = endGame rName "goodWins"
    | roundNum == 5         = endGame rName "roundLimit"
    | otherwise = do
        makeRound rName
        -- DB Query ----------------------------------
        cTeamEvil <- getPlayerRolesCount rName False
        cTeamGood <- getPlayerRolesCount rName True
        ----------------------------------------------
        -- make request to the front, saying that need to fresh the data
        game rName (roundNum + 1) cTeamEvil cTeamGood

-- Start the game
data StartGameResult = GameStarted | NotRoomMaster String | RoomAlreadyUp String
startGame :: String -> String -> IO StartGameResult
startGame rName pName = do
    roomMaster <- isRoomMaster rName pName

    if roomMaster
        then do
            isRoomUp <- getRoomUpState rName

            if not isRoomUp
                then do
                    roomPlayers     <- getRoomPlayersCount rName
                    let nPlayers    = 12 - roomPlayers

                    createBots          nPlayers rName 
                    addPlayersToGame    rName
                    distributeRoles     rName
                    setRoomUpState      rName True

                    putStrLn $ "> [" ++ rName ++ "] Room - jogo começou!"
                    game rName 0 6 6
                    return GameStarted
                else do
                    let errMsg = "[" ++ (rName) ++ "] Room - jogo já começou"
                    putStrLn errMsg
                    return (RoomAlreadyUp errMsg)

        else do
            let errMsg = "[" ++ (rName) ++ "] Room - Você não é o room master."
            putStrLn errMsg
            return (NotRoomMaster errMsg)


-- Finish the game
endGame :: String -> String -> IO ()
endGame rName reason = do
    putStrLn $ ("> [" ++ (rName) ++ "] Room - o jogo  acabou!")
    deleteUserGameData      rName
    deleteRoomBots          rName
    resetPlayersCurrentRoom rName
    deleteRoom              rName

-- Make the Game Rounds
makeRound :: String -> IO ()
makeRound rName = runRound rName

-- Vote for a player
vote :: String -> String -> IO ()
vote pName pName_voted = incrementVote pName pName_voted

-- Make an Round Action
makeAction :: String -> String -> String -> IO ()
makeAction agent action action_reciever 
    | action == "vote"          = vote      agent action_reciever 
    | action == "kill"          = kill      agent action_reciever
    | action == "save"          = save      agent action_reciever 
    | action == "search"        = search    agent action_reciever 
    | action == "reveal"        = reveal    agent action_reciever 
    | action == "silence"       = silence   agent action_reciever 
    | action == "paralize"      = paralize      agent action_reciever 
    | action == "curse_word"    = setCursedWord agent action_reciever 
    | otherwise                 = putStrLn $ "Invalid Action"

-- Sebnd a message to a chat
makeMessage :: String -> String -> IO ()
makeMessage pName msg = sendMessage pName msg
