module GameUtils.ChatFunctions where

import Core.DbFunctions
import GameUtils.RoundUtils
import GameUtils.GameFunctions
import GameUtils.GameStartFunctions

import qualified Data.ByteString.Char8 as BS2

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), decode, encode, object)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))



sendMessage :: String -> String -> IO ()
sendMessage pName msg = do
    pUUID   <- getUUIDFromPlayerName pName
    rName   <- getPlayerRoomName pUUID
    rState  <- getRoomRoundState rName

    if rState == "voteRound"
        then do
            conn    <- getDbConnection

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "UPDATE Room SET round_messages = round_messages || ARRAY[?] WHERE room_name = ?"
            _ <- execute conn sqlQuery (msg, rName)
            ----------------------------------------------
            close conn

            -- send a request to the react server    
            putStrLn $ "> Mensagem [" ++ msg ++ "] enviada no Room [" ++ rName ++ "]"
        else do
            putStrLn $ "> Room [" ++ rName ++ "] não está num round de votação"


-- Get the last message idx
getLastMessageIdx :: String -> IO Int
getLastMessageIdx rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT array_length(round_messages, 1) FROM Room WHERE room_name = ? "
    result <- query conn sqlQuery (Only rName) :: IO [Only Int]
    ----------------------------------------------
    close conn

    return $ fromOnly (head result)


-- Get the messages 
getMessagesListFromRoom :: String -> Int -> IO [String]
getMessagesListFromRoom rName lastIdxPlayer = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT round_messages[?:array_length(round_messages, 1)] FROM Room WHERE room_name = ?"
    result <- query conn sqlQuery (lastIdxPlayer, rName) :: IO [Only String]
    ----------------------------------------------
    close conn

    return $ map fromOnly result
