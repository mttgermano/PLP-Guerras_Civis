module GameUtils.BotLogic where

import Core.DbFunctions
import GameUtils.GameFunctions
import GameUtils.GameFunctionsInit
import GameUtils.GameRoundFunctions
import LoginUtils.LoginPlayerFunctions

import qualified Data.ByteString.Char8 as BS2

import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Control.Monad (forM)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))

import System.Random


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

    let resultado = listSom comp references
    let ind = biggestVote resultado

    close conn

    bName <- getPlayerFromID botUuid
    let playerToIncrement = players !! ind
    incrementVote bName playerToIncrement


compareIsGood :: String -> [Int] -> String -> IO Int
compareIsGood botId roles playerId = do
    botIsGood       <- getIsGood botId
    playerIsGood    <- getIsGood playerId
    playerRole      <- getRole playerId

    if (botIsGood /= playerIsGood) && (playerRole `elem` roles)
        then    return 1000000
    else if ((botIsGood == playerIsGood) && (playerRole `elem` roles))
        then    return (-100000)
    else        return 0


compareIsGoodList :: String -> [String] -> [Int] -> IO [Int]
compareIsGoodList botId playerIds roles = mapM (compareIsGood botId roles) playerIds


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
