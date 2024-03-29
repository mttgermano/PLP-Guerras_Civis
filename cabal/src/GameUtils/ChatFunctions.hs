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
    conn    <- getDbConnection
    pUUID   <- getUUIDFromPlayerName pName
    rName   <- getPlayerRoomName pUUID

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE Room SET round_messages = round_messages || ARRAY[?] WHERE room_name = ?"
    _ <- execute conn sqlQuery (msg, rName)
    ----------------------------------------------
    close conn

    -- send a request to the react server    
    putStrLn $ "> Mensagem [" ++ msg ++ "] enviada no Room [" ++ rName ++ "]"
