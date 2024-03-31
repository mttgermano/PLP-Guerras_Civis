module Game.BotLogic where

import Core.DbFunctions
import Game.GameFunctions
import Game.StartFunctions
import Game.RoundFunctions
import Game.RoleFunctions
import Game.ChatFunctions
import LoginUtils.PlayerFunctions
import Utils.Utils

import qualified Data.ByteString.Char8 as BS2

import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Control.Monad (forM)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))

import System.Random
import GHC.Base (IO)

-- Randomly choose a player who is alive to perform the action.
botActionChoice :: String -> IO String
botActionChoice rName = do
    players <- getRoomPlayersUUIDList rName

    posicao <- randomRIO (0, length players - 1)
    let player_uuid = players !! posicao
    alive <- isPlayerAlive player_uuid 
    
    if alive
        then return player_uuid

        else botActionChoice rName

-- Create the bots in a room
createBots :: Int -> String -> IO ()
createBots quant rName
    | quant <= 0    = putStrLn $ ("> All Bots created in [" ++ rName ++ "]")
    | otherwise     = do
        uuid <- fmap toString nextRandom    -- Generate random UUID
        conn <- getDbConnection

        let bot_Name    = "bot-" ++ take 4 uuid
        rUuid           <- getRoomUuid rName

        let newBot  = Player {
            isBot   = True, 
            pId     = uuid, 
            pName   = bot_Name, 
            pPassword   = "botpasswd", 
            currentRoom = Just rUuid
        }

        -- DB Query ----------------------------------
        let sqlQuery = Query $ BS2.pack "INSERT INTO Player (is_bot ,player_uuid, player_name, player_password, current_room) VALUES (?, ?, ?, ?, ?)"
        _ <- execute conn sqlQuery newBot
        ----------------------------------------------
        close conn
        createBots (quant-1) rName

-- Separete all strings by space
splitBySpaces :: [String] -> [String]
splitBySpaces = concatMap words

-- Choose the most mentioned player in the chat and vote for him.
botBrain :: String -> String -> IO ()
botBrain rName botUuid = do
    players <- getRoomPlayersUUIDList rName
    playersNames <- getPlayersNames players
    messages <- getMessagesListFromRoom rName 0

    let allWords = splitBySpaces messages
        references = countReferencesForAll allWords playersNames

    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT who_is_known FROM RoleKnowledge WHERE who_knows = ?"
    roles <- forM players $ \player -> do
        [Only role] <- query conn sqlQuery (Only botUuid)
        return role
    ----------------------------------------------

    let comparation = compareIsGoodList botUuid players roles
    comp <- comparation
    let results = listSom comp references

    let ind = biggestVote results

    close conn

    bName <- getPlayerNameFromUUID botUuid
    let playerToIncrement = players !! ind
    incrementVote bName playerToIncrement

-- Verify whether someone is on the opposite team of the bot and whether they are alive.
compareIsGoodIsAlive :: String -> [String] -> String -> IO Int
compareIsGoodIsAlive botId players playerId = do
    botIsGood       <- getIsGood botId
    playerIsGood    <- getIsGood playerId
    playerAlive     <- isPlayerAlive playerId

    if (botIsGood /= playerIsGood) && (playerId `elem` players) && playerAlive
        then    return 1000000
    else if ((botIsGood == playerIsGood) && (playerId `elem` players)) || not playerAlive
        then    return (-100000)
    else        return 0

-- Call compareIsGoodIsAlive for every player.
compareIsGoodList :: String -> [String] -> [String] -> IO [Int]
compareIsGoodList botId playerIds players = mapM (compareIsGoodIsAlive botId players) playerIds

-- Take the biggest voted player
biggestVote :: Ord a => [a] -> Int
biggestVote []      = 0
biggestVote list    = biggestIdxAux list 0 0
  where
    biggestIdxAux [] _ _ = 0
    biggestIdxAux (x:xs) idx maiorIndiceAtual
      | x > (list !! maiorIndiceAtual)  = biggestIdxAux xs (idx + 1) idx
      | otherwise                       = biggestIdxAux xs (idx + 1) maiorIndiceAtual

-- Call nameCountReferences for every player
countReferencesForAll :: [String] -> [String] -> [Int]
countReferencesForAll _ [] = []
countReferencesForAll words (x:xs) = nameCountReferences x words : countReferencesForAll words xs

-- Som two lists
listSom :: [Int] -> [Int] -> [Int]
listSom [] [] = []
listSom [] ys = ys
listSom xs [] = xs
listSom (x:xs) (y:ys) = (x + y) : listSom xs ys

-- Count the times a player's name was written in the chat.
nameCountReferences :: String -> [String]-> Int
nameCountReferences player playersNames
    | null playersNames = 0
    | otherwise         = length (filter(== player)playersNames)

-- Words to be chosen as cursed word by the bot 
possibleWords :: [String]
possibleWords = ["sinto", "acho", "teste", "livro", "água", "banana", "futebol", "computador", "amor", "tempo", "cidade", "felicidade"]

-- Function to choose a random word
randomWord :: IO String
randomWord = do
    index <- randomRIO (0, length possibleWords - 1)
    return (possibleWords !! index)


-- Call the action of the bot
botAction :: String -> String -> IO ()
botAction botId rName = do
    botRole        <- getRole botId
    playerId       <- botActionChoice rName
    choiceWord <- randomWord

    case botRole of
        1 -> kill botId playerId
        2 -> apprentice botId playerId
        3 -> reveal botId playerId
        4 -> paralize botId playerId
        5 -> silence botId playerId
        6 -> setCursedWord botId choiceWord
        7 -> search botId playerId
        8 -> kill botId playerId
        9 -> police botId playerId
        10 -> save botId playerId
        12 -> revenge botId playerId

-- Call botAction for every bot
callBots :: [String] -> String -> IO ()
callBots arr rName = mapM_ (\botId -> botAction botId rName) arr

-- Take the bots of the room and call the actions
botsRound :: String -> IO ()
botsRound rName = do
    bots <- getRoomBots rName
    callBots bots rName


-- Deletes all bots, after the game ended
deleteRoomBots :: String -> IO ()
deleteRoomBots rName = do
    bList <- getRoomBots rName
    mapM_ deleteBot bList
    putStrLn $ "> [" ++ rName ++ "] Room - bots deletados"


-- Delete a single bot
deleteBot :: String -> IO ()
deleteBot bUUID = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "DELETE FROM Player WHERE player_uuid = ?"
    _ <- execute conn sqlQuery (Only bUUID)
    ----------------------------------------------
    close conn

