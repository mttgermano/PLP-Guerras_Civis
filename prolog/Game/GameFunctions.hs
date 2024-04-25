module Game.GameFunctions where

import Core.DbFunctions
import Game.StartFunctions
import Utils.Utils

import qualified Data.ByteString.Char8 as BS2
import Control.Concurrent (threadDelay)
import GHC.Generics

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))
import GHC.Base (IO)



-- User Data Type
data UserGame = UserGame { 
    pId_ :: String
} deriving (Show, Generic)

-- Convert User into a tuple for SQL insert
instance ToRow UserGame where
    toRow userGame = [toField (pId_ userGame)]


sleep :: Int -> IO ()
sleep minutes = do
    let delay = minutes * (60 * 1000000)
    threadDelay delay

-- Increment the vote for a user in the db
incrementVote :: String -> String -> IO ()   
incrementVote pName pName_voted = do
    allowed <- isAllowed pName "vote"
    if allowed 
        then do
            conn    <- getDbConnection
            pUUID   <- getUUIDFromPlayerName pName_voted

            -- DB Query ----------------------------------
            let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET votes = votes + 1 WHERE player_uuid = ?"
            _ <- execute conn sqlQuery (Only pUUID)
            ----------------------------------------------
            putStrLn $ ("> Voto incrementado para user [" ++ (pName_voted) ++ "]")
            close conn
        else
            putStrLn $ ("> [" ++ pName ++ "] is silenced")
