module GameRoundFunctions where
import DbFunctions

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))


-- Get the list of all evil players in a room
getRoomPlayersEvil :: String -> IO [String]
getRoomPlayersEvil rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "\
               \SELECT u.player_uuid \
               \FROM Player p \
               \JOIN UserGameData u ON p.player_uuid = u.player_uuid \
               \WHERE p.current_room = ?;"    
    result <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
    return $ map (\(Only player_uuid) -> player_uuid) result


-- Get the list of all good players in a room
getRoomPlayersGood :: String -> IO [String]
getRoomPlayersGood rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "\
               \SELECT u.player_uuid \
               \FROM Player p \
               \JOIN UserGameData u ON p.player_uuid = u.player_uuid \
               \WHERE p.current_room = ?;"    
    result <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
    return $ map (\(Only player_uuid) -> player_uuid) result


-- Run the evil guys round
actionEvilRound :: String -> IO ()
actionEvilRound rName = do
    putStrLn $ "Começando Round Mafiosos no " ++ (rName)
    evil_list <- getRoomPlayersEvil rName

    -- send a request to the front, so it will allert that the user can make an action
    -- deixa rodar por 2 min
    -- desfaz a acao
    putStrLn $ "Terminou Round Mafiosos no " ++ (rName)


-- Run the good guys round
actionGoodRound :: String -> IO ()
actionGoodRound rName = do
    putStrLn $ "Começando Round Civis no " ++ (rName)
    -- send a request to the front, so the bad guys can make an action
    good_list <- getRoomPlayersGood rName
    -- send a request to the front, so it will allert that the user can make an action
    -- deixa rodar por 2 min
    -- desfaz a acao
    putStrLn $ "> Terminou Round Civis no " ++ (rName)
