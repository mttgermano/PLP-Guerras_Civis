module Game.RoundFunctions where

import Control.Monad (forM_)
import Core.DbFunctions
import Data.ByteString.Char8 qualified as BS2
import Data.Function (on)
import Data.List (sortBy)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query (Query))
import GHC.Base (IO)
import Game.GameFunctions
import Game.StartFunctions
import Utils.Utils

-- Update the round state of the Room
updateRoundState :: String -> String -> IO ()
updateRoundState room_name state = do
  conn <- getDbConnection

  -- DB Query change current room --------------
  let sqlQuery = Query $ BS2.pack "UPDATE Room SET round_state = ? WHERE room_name = ?"
  _ <- execute conn sqlQuery (state, room_name)
  ----------------------------------------------
  close conn

-- Run the evil guys round
actionRound :: String -> IO ()
actionRound rName = do
  putStrLn $ ("> [" ++ (rName) ++ "] Room - Começando Action Round ")
  updateRoundState rName "action"

  evilList <- getRoomPlayersGoodness rName False
  goodList <- getRoomPlayersGoodness rName True

  sleep 1

  putStrLn $ ("> [" ++ (rName) ++ "] Room - Terminou  Action Round")

-- -- Reset all room players atributes
-- resetRoomPlayersAtributes :: String -> IO ()
-- resetRoomPlayersAtributes rName = do
--     conn    <- getDbConnection
--     pList   <- getRoomPlayersUUIDList rName
--     let atributes = ["votes", "kill_vote", "is_paralized", "is_silenced"]
--
--     -- For each pUUID and attribute, execute the SQL query
--     forM_ pList $ \pUUID ->
--         forM_ atributes $ \atribute -> do
--             let sqlQuery = Query $ BS2.pack $ "UPDATE UserGameData SET " ++ atribute ++ " = 0 WHERE player_uuid = ?"
--             _ <- execute conn sqlQuery (Only pUUID)
--             return ()
--     close conn

roundResult :: String -> IO ()
roundResult rName = do
  killedPlayers rName

killedPlayers :: String -> IO ()
killedPlayers rName = do
  conn <- getDbConnection
  -- DB Query ----------------------------------
  let sqlQuery = Query $ BS2.pack $ "SELECT player_uuid FROM UserGameData WHERE kill_vote > 0 AND is_alive = True"
  players <- query_ conn sqlQuery :: IO [Only String]
  ----------------------------------------------

  -- Processing each player with kill_vote > 0 --
  mapM_ (\(Only playerUUID) -> killPlayer playerUUID) players
  close conn

killPlayer :: String -> IO ()
killPlayer playerUUID = do
  conn <- getDbConnection
  -- DB Query ----------------------------------
  let sqlQuery = Query $ BS2.pack $ "UPDATE UserGameData SET is_alive = False WHERE player_uuid = ?"
  _ <- execute conn sqlQuery (Only playerUUID)
  ----------------------------------------------
  putStrLn $ "User " ++ playerUUID ++ " morreu."
  rName <- getPlayerRoomName playerUUID
  pName <- getPlayerNameFromUUID playerUUID
  let message = "User " ++ pName ++ " morreu."
  admSendMessage rName message

computeVote :: IO ()
computeVote = do
  conn <- getDbConnection
  -- DB Query ----------------------------------
  let sqlQuery = Query $ BS2.pack $ "SELECT player_uuid FROM UserGameData WHERE is_alive = ?"
  players <- query conn sqlQuery (Only True) :: IO [Only String]
  ----------------------------------------------
  close conn
  -- Calculate votes for each player
  votes <- mapM (\(Only pUuid) -> countVotes pUuid) players

  -- Find the player with the most votes
  let maxVotes = maximum votes
      maxVotePlayers = filter (\(p, v) -> v == maxVotes) (zip players votes)
  -- If everyone has 0 votes or multiple players have the same maximum votes, do nothing
  if maxVotes == 0 || length maxVotePlayers /= 1
    then return ()
    else let (Only maxPlayer, _) : _ = maxVotePlayers in killPlayer maxPlayer

countVotes :: String -> IO Int
countVotes pUuid = do
  conn <- getDbConnection
  -- DB Query ----------------------------------
  let sqlQuery = Query $ BS2.pack $ "SELECT votes FROM UserGameDAta WHERE player_uuid = ?"
  [Only votes] <- query conn sqlQuery (Only pUuid) :: IO [Only Int]
  close conn
  return votes

clearRound :: String -> IO ()
clearRound rName = do
  playersUuid <- getRoomPlayersUUIDList rName
  forM_ playersUuid clearPlayer

clearPlayer :: String -> IO ()
clearPlayer pUuid = do
  conn <- getDbConnection
  -- DB Query ----------------------------------
  let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET kill_vote = 0 WHERE player_uuid = ?"
  _ <- execute conn sqlQuery (Only pUuid)
  ---------------------------------------------
  -- DB Query ----------------------------------
  let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET votes = 0 WHERE player_uuid = ?"
  _ <- execute conn sqlQuery (Only pUuid)
  ---------------------------------------------
  -- DB Query ----------------------------------
  let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET is_paralized = 0 WHERE player_uuid = ?"
  _ <- execute conn sqlQuery (Only pUuid)
  ---------------------------------------------
  -- DB Query ----------------------------------
  let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET is_silenced = 0 WHERE player_uuid = ?"
  _ <- execute conn sqlQuery (Only pUuid)
  ---------------------------------------------
  close conn
