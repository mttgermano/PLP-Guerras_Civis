module BotLogic where

import DbFunctions
import RoomFunctions

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))



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



nameCount :: String -> INT -> INT -> IO ()
nameCount nomes tam at = do

botBrain :: String -> IO ()
botBrain rName = do
    players <- getRoomPlayers rName

    playersNames <- getPlayersNames players


    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT role_idx FROM UserGameData WHERE user_id = ?"
    result <- Query conn sqlQuery (Only pName_voted)
    ----------------------------------------------
    putStrLn $ "> Vote incremented for user [" ++ (pName_voted) ++ "]"
    close conn


    vote botName usuario
