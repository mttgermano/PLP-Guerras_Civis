module BotLogic where

import DbFunctions
import RoomFunctions

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
        else gerarNumeroValido rName


createBots :: Int -> String -> IO ()
createBots quant rName = do
    | quant == 0 = return
    | otherwise = do
        loginRoom 
        uuid <- fmap toString nextRandom    -- Generate random UUID
        conn <- getDbConnection
        let bot_Name = "bot" + uuid[0] + uuid[1] + uuid[2] + uuid[3] 
        let newBot = Player { isBot = True, pId = uuid, pName = bot_Name, pPassword = Nothing, currentRoom = rName}

        -- DB Query ----------------------------------
        let sqlQuery = Query $ BS2.pack "INSERT INTO Player (is_bot ,player_uuid, player_name, player_password, current_room) VALUES (?, ?, ?, ?, ?)"
        _ <- execute conn sqlQuery newBot
        ----------------------------------------------
        close conn
        putStrLn $ "Bot created: " ++ show newBot
        createBots quant-1 rName


isInRoles :: [UUID] -> Role -> UUID -> Bool
isInRoles roles role uuid = any (\roleUuid -> uuid == roleUuid && role uuid) roles


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


    vote botName players[ind]


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
