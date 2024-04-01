module Controllers.GameController where

import Controllers.RoundController
import Game.StartFunctions
import Game.StopFunctions
import Game.GameFunctions
import Game.RoleFunctions
import Game.ChatFunctions
import Game.RoundFunctions
import Game.BotLogic
import Utils.Utils
import Game.GameFunctions



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

                    updateRoundState                    rName "action"
                    createBots                          nPlayers rName 
                    addPlayersToGame                    rName
                    distributeRoles                     rName
                    distributePlayersInitialKnowledge   rName
                    setRoomUpState                      rName True

                    let message = "> [" ++ rName ++ "] Room - jogo começou!"
                    putStrLn $ "> [" ++ rName ++ "] Room - jogo começou!"
                    admSendMessage rName message

                    return GameStarted
                else do
                    let errMsg = "[" ++ (rName) ++ "] Room - jogo já começou"
                    putStrLn errMsg
                    return (RoomAlreadyUp errMsg)

        else do
            let errMsg = "[" ++ (rName) ++ "] Room - Você não é o room master."
            putStrLn errMsg
            return (NotRoomMaster errMsg)

-- Run Action Round
runActionRound :: String -> Int -> IO ()
runActionRound rName roundNum = do
    actionRound       rName
    botsRound         rName
    roundResult       rName
    checkEndGame      rName roundNum

-- Run Vote Round
runVoteRound :: String -> Int -> IO ()
runVoteRound rName roundNum = do
    voteRound           rName
    voteBotsRound       rName
    computeVote         rName
    clearRound          rName
    checkEndGame        rName roundNum

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

-- Vote for a player
vote :: String -> String -> IO ()
vote pName pName_voted = incrementVote pName pName_voted

-- Make an Round Action
makeAction :: String -> String -> IO ()
makeAction agent action_reciever = do
    agentID <- getUUIDFromPlayerName    agent
    role    <- getRole                  agentID
    rName   <- getPlayerRoomName        agentID
    rState  <- getRoomRoundState        rName

    if rState == "vote"
        then do
            vote          agent action_reciever
        else do
            case role of
                1  -> kill              agent action_reciever
                2  -> apprentice        agent action_reciever
                3  -> reveal            agent action_reciever
                4  -> paralize          agent action_reciever
                5  -> silence           agent action_reciever
                6  -> setCursedWord     agent action_reciever
                7  -> search            agent action_reciever
                8  -> kill              agent action_reciever
                9  -> police            agent action_reciever
                10 -> save              agent action_reciever
                12 -> revenge           agent action_reciever
                _  -> putStrLn $ "Invalid Action"


-- Sebnd a message to a chat
makeMessage :: String -> String -> IO ()
makeMessage pName msg = sendMessage pName msg
