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


botBrain :: String -> String -> IO ()
botBrain rName messages = do
    players <- getRoomPlayers rName

    playersNames <- getPlayersNames players

    let allWords = words  messages
    let references = countReferencesForAll allWords playersNames
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT role_idx FROM UserGameData WHERE user_id IN ?"
    roles <- Query conn sqlQuery (Only $ In playerIds)
    ----------------------------------------------
    putStrLn $ "> Vote incremented for user [" ++ (pName_voted) ++ "]"
    close conn


    vote botName usuario


countReferencesForAll :: String -> [String] -> [Int]
countReferencesForAll _ [] = []
countReferencesForAll names (x:xs) = nameCountReferences x names : countReferencesForAll names xs


nameCountReferences :: String -> [String]-> Int
nameCountReferences player playersNames
    | null playersNames = 0
    | otherwise = length (filter(== player)playersNames)
