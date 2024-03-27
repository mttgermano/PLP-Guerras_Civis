module GameUtils.BotLogic where

import Core.DbFunctions
import GameUtils.GameFunctions
import GameUtils.GameStartFunctions
import GameUtils.RoundFunctions
import GameUtils.RoleFunctions
import LoginUtils.PlayerFunctions

import qualified Data.ByteString.Char8 as BS2

import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Control.Monad (forM)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))

import System.Random
import GHC.Base (IO)


botActionChoice :: String -> IO String
botActionChoice rName = do
    players <- getRoomPlayers rName

    posicao <- randomRIO (0, length players - 1)
    let player_uuid = players !! posicao
    alive <- isPlayerAlive player_uuid  -- Unwrap the result from IO monad
    
    if alive
        then return player_uuid

        else botActionChoice rName


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
        putStrLn $ "> Bot created: " ++ show newBot
        createBots (quant-1) rName


botBrain :: String -> String -> String -> IO ()
botBrain rName messages botUuid = do
    players <- getRoomPlayers rName
    playersNames <- getPlayersNames players

    let allWords = words messages
        references = countReferencesForAll allWords playersNames

    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT role_idx FROM UserGameData WHERE user_id IN ?"
    roles <- forM players $ \player -> do
        [Only role] <- query conn sqlQuery (Only player)
        return role
    ----------------------------------------------

    let comparation = compareIsGoodList botUuid players roles
    comp <- comparation
    let results = listSom comp references

    let ind = biggestVote results

    close conn

    bName <- getPlayerFromID botUuid
    let playerToIncrement = players !! ind
    incrementVote bName playerToIncrement


compareIsGoodIsAlive :: String -> [Int] -> String -> IO Int
compareIsGoodIsAlive botId roles playerId = do
    botIsGood       <- getIsGood botId
    playerIsGood    <- getIsGood playerId
    playerRole      <- getRole playerId
    playerAlive     <- isPlayerAlive playerId

    if (botIsGood /= playerIsGood) && (playerRole `elem` roles) && playerAlive
        then    return 1000000
    else if ((botIsGood == playerIsGood) && (playerRole `elem` roles)) || not playerAlive
        then    return (-100000)
    else        return 0


compareIsGoodList :: String -> [String] -> [Int] -> IO [Int]
compareIsGoodList botId playerIds roles = mapM (compareIsGoodIsAlive botId roles) playerIds


biggestVote :: Ord a => [a] -> Int
biggestVote []      = 0
biggestVote list    = biggestIdxAux list 0 0
  where
    biggestIdxAux [] _ _ = 0
    biggestIdxAux (x:xs) idx maiorIndiceAtual
      | x > (list !! maiorIndiceAtual)  = biggestIdxAux xs (idx + 1) idx
      | otherwise                       = biggestIdxAux xs (idx + 1) maiorIndiceAtual


countReferencesForAll :: [String] -> [String] -> [Int]
countReferencesForAll _ [] = []
countReferencesForAll words (x:xs) = nameCountReferences x words : countReferencesForAll words xs

listSom :: [Int] -> [Int] -> [Int]
listSom [] [] = []
listSom [] ys = ys
listSom xs [] = xs
listSom (x:xs) (y:ys) = (x + y) : listSom xs ys


nameCountReferences :: String -> [String]-> Int
nameCountReferences player playersNames
    | null playersNames = 0
    | otherwise         = length (filter(== player)playersNames)




possibleWords :: [String]
possibleWords = ["matou", "acho", "", "livro", "água", "banana", "futebol", "computador", "verde", "amor", "tempo", "cidade", "música", "felicidade"]

-- Function to generate a random Portuguese word
randomWord :: IO String
randomWord = do
    index <- randomRIO (0, length possibleWords - 1)
    return (possibleWords !! index)



-- Function to test each element
botAction :: String -> String -> IO ()
botAction botId rName = do
    botRole        <- getRole botId
    playerId       <- botActionChoice rName
    choiceWord <- randomWord

    case botRole of
        1 -> kill botId playerId
        2 -> kill botId playerId
        3 -> reveal botId playerId
        4 -> paralize botId playerId
        5 -> silence botId playerId
        6 -> setCursedWord botId choiceWord
        7 -> search botId playerId
        8 -> kill botId playerId
        9 -> kill botId playerId
        10 -> save botId playerId



callBots :: [String] -> String -> IO ()
callBots arr rName = mapM_ (\botId -> botAction botId rName) arr




botsRound :: Bool -> String -> IO ()
botsRound good rName = do
    bots <- getRoomBotsGoodness rName good
    callBots bots rName
