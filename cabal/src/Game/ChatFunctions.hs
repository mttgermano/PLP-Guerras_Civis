module Game.ChatFunctions where

import Core.DbFunctions
import Game.GameFunctions
import Game.StartFunctions
import Game.RoundFunctions
import Utils.Utils

import qualified Data.ByteString.Char8 as BS2

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), decode, encode, object)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe (mapMaybe, Maybe (Nothing))


import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))


sendMessage :: String -> String -> IO ()
sendMessage pName msg = do
    pUUID   <- getUUIDFromPlayerName pName
    rName   <- getPlayerRoomName pUUID
    rState  <- getRoomRoundState rName
    allowed <- isSilenced pUUID
    canSend <- canSendMessage pName msg

    if allowed == 0 && canSend
        then do
            conn    <- getDbConnection

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "UPDATE Room SET round_messages = round_messages || ARRAY[?] WHERE room_name = ?"
            _ <- execute conn sqlQuery (msg, rName)
            ----------------------------------------------
            close conn

            -- send a request to the react server    
            putStrLn $ "> [" ++ rName ++ "] Room - recebeu mensagem [" ++ msg ++ "]"
        else do
            putStrLn $ "> [" ++ pName ++ "] Player está silenciado"


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
    let sqlQuery = Query $ BS2.pack "SELECT unnest(round_messages) FROM Room WHERE room_name = ?"
    result <- query conn sqlQuery (Only rName) :: IO [Only String]
    close conn
    return $ map fromOnly result


canSendMessage :: String -> String -> IO Bool
canSendMessage pName msg = do
    pUUID <- getUUIDFromPlayerName pName
    rName <- getPlayerRoomName pUUID
    isCursed <- hasCursedWord rName
    if isCursed
        then do
            cursedWord <- getCursedWord rName
            case cursedWord of
                Just word -> do
                    let isSub = isSubstring word msg
                    if isSub
                        then do
                            killPlayer pUUID
                            return False
                        else
                            return True
                Nothing -> return True
        else
            return True





isSubstring :: String -> String -> Bool
isSubstring [] _ = True
isSubstring _ [] = False
isSubstring (x:xs) (y:ys)
    | x == y    = isPrefix xs ys || isSubstring (x:xs) ys
    | otherwise = isSubstring (x:xs) ys

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys)
    | x == y    = isPrefix xs ys
    | otherwise = False
