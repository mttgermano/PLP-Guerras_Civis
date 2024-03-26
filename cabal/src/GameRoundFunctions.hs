module GameRoundFunctions where
import DbFunctions
import GameFunctions
import GameFunctionsInit

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))



-- It selects the itens of the first list whom index, in the second list is equal to flag
selectTheIndex :: [String] -> [Bool] -> Bool -> [String]
selectTheIndex pList gList b = [x | (x, flag) <- zip pList gList, flag == b]

-- Get the list of all good players in a room
getRoomPlayersGoodness :: String -> Bool -> IO [String]
getRoomPlayersGoodness rName isGood = do
    rPlayers        <- getRoomPlayers rName
    goodnessList    <- mapM getIsGood rPlayers

    return $ selectTheIndex rPlayers goodnessList isGood


-- Run the evil guys round
actionEvilRound :: String -> IO ()
actionEvilRound rName = do
    putStrLn $ ("> Começando Round Mafiosos [" ++ (rName) ++ "]")
    evilList <- getRoomPlayersGoodness rName False

    -- send a request to the front, so it will allert that the user can make an action
    -- deixa rodar por 2 min
    -- desfaz a acao
    putStrLn $ ("> Terminou Round Mafiosos [" ++ (rName) ++ "]")


-- Run the good guys round
actionGoodRound :: String -> IO ()
actionGoodRound rName = do
    putStrLn $ ("> Começando Round Civis [" ++ (rName) ++ "]")
    goodList <- getRoomPlayersGoodness rName True

    -- send a request to the front, so it will allert that the user can make an action
    -- deixa rodar por 2 min
    -- desfaz a acao
    putStrLn $ ("> Terminou Round Civis [" ++ (rName) ++ "]")


-- Get player name from id
getPlayerFromID :: String -> IO (Maybe String)
getPlayerFromID playerId = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "\
               \SELECT p.player_name \
               \FROM Player p \
               \WHERE p.player_uuid = ?"
    result <- query conn sqlQuery (Only playerId)
    ----------------------------------------------
    case result of
        [Only playerName] -> return (Just playerName)
        _ -> return Nothing


-- Return a list with the players names
getPlayersNames :: [String] -> IO [String]
getPlayersNames [] = return []
getPlayersNames (id:ids) = do
    maybeName   <- getPlayerFromID (show id)
    rest        <- getPlayersNames ids

    return $ case maybeName of
        Just name -> name : rest
        Nothing   -> rest
