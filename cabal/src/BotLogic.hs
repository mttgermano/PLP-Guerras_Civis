module BotLogic where

import DbFunctions
import GameFunctions
import GameRoleFunctions
import GameFunctionsInit
import GameRoundFunctions
import LoginPlayerFunctions

import qualified Data.ByteString.Char8 as BS2

import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))

import System.Random


botActionChoice :: String -> IO String
botActionChoice rName = do
    players <- getRoomPlayers rName

    posicao <- randomRIO (0, length players - 1)
    let player_uuid = players !! posicao
    if isPlayerAlive player_uuid
        then return (player_uuid)
        else botActionChoice rName


createBots :: Int -> String -> IO ()
createBots quant rName
    | quant == 0    = return
    | otherwise     = do
        uuid <- fmap toString nextRandom    -- Generate random UUID
        conn <- getDbConnection

        let bot_Name = "bot" ++ take 4 uuid
        let newBot = Player { isBot = True, pId = uuid, pName = bot_Name, pPassword = "evertonquero10", currentRoom = Just rName}

        -- DB Query ----------------------------------
        let sqlQuery = Query $ BS2.pack "INSERT INTO Player (is_bot ,player_uuid, player_name, player_password, current_room) VALUES (?, ?, ?, ?, ?)"
        _ <- execute conn sqlQuery newBot
        ----------------------------------------------
        close conn
        putStrLn $ "Bot created: " ++ show newBot
        createBots quant-1 rName


botBrain :: String -> String -> String -> IO ()
botBrain rName messages botUuid = do
    players <- getRoomPlayers rName

    playersNames <- getPlayersNames players

    let isGood = getIsGood playerUUID
    let allWords = words  messages
    let references = countReferencesForAll allWords playersNames
    

    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT role_idx FROM UserGameData WHERE user_id IN ?"
    roles <- Query conn sqlQuery (Only $ In playerIds)
    ----------------------------------------------
    let comparation = compareIsGoodList botUuid players roles

    let resultado = listSom comparation references
    let ind = biggestVote resultado

    putStrLn $ "> Vote incremented for user [" ++ (pName_voted) ++ "]"
    close conn


    incrementVote botName players[ind]


compareIsGood :: String -> [String] -> String -> Int
compareIsGood botId roles playerId
    | getIsGood botId /= getIsGood playerId && playerId `elem` roles = 1000000
    | otherwise = 0

compareIsGoodList :: String -> [String] -> [String] -> [Int]
compareIsGoodList botId playerIds roles = map (compareIsGood botId roles) playerIds


biggestVote :: Ord a => [a] -> Maybe Int
biggestVote [] = Nothing
biggestVote lista = Just (maiorIndiceAux lista 0 0)
  where
    maiorIndiceAux [] _ _ = 0
    maiorIndiceAux (x:xs) indice maiorIndiceAtual
      | x > (lista !! maiorIndiceAtual) = maiorIndiceAux xs (indice + 1) indice
      | otherwise = maiorIndiceAux xs (indice + 1) maiorIndiceAtual



countReferencesForAll :: String -> [String] -> [Int]
countReferencesForAll _ [] = []
countReferencesForAll names (x:xs) = nameCountReferences x names : countReferencesForAll names xs

listSom :: [Int] -> [Int] -> [Int]
listSom [] [] = []
listSom [] ys = ys
listSom xs [] = xs
listSom (x:xs) (y:ys) = (x + y) : listSom xs ys



nameCountReferences :: String -> [String]-> Int
nameCountReferences player playersNames
    | null playersNames = 0
    | otherwise = length (filter(== player)playersNames)
