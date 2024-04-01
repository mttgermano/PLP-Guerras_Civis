module Controllers.RoundController where

import GHC.Generics (Constructor (conFixity))
import Game.BotLogic
import Game.ChatFunctions
import Game.GameFunctions
import Game.RoundFunctions
import Game.StartFunctions
import Utils.Utils

-- Run the Round which the users can vote
voteRound :: String -> IO ()
voteRound rName = do
  putStrLn $ ("> [" ++ (rName) ++ "] Room - Começando Vote   Round ")
  updateRoundState rName "vote"

  sleep 1

  putStrLn $ ("> [" ++ (rName) ++ "] Room - Terminou  Vote   Round")
  putStrLn $ replicate 50 '-'

-- Run all the sequence of rounds
runRound :: String -> IO ()
runRound rName = do
  updateRoundState rName "action"
  -- roundDefaultSettings rName
  actionRound rName
  botsRound rName
  -- getRoomActionsResults   rName
  roundResult rName -- --> se X ta com kill_vote 1, print: User morreu y, coloca is_alive pra 0, ...
  voteRound rName
  --sendMessage "john" "o culpado e john"
  voteBotsRound rName
  computeVote
  clearRound rName

-- Reset all the setting of a Room
-- roundDefaultSettings :: String -> IO ()
-- roundDefaultSettings rName = do
--   updateRoundState rName "startRound"

-- -- resetRoundMessages              rName
-- -- resetRoomPlayersAtributes       rName
