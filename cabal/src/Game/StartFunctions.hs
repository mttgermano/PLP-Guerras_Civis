{-# LANGUAGE DeriveGeneric #-}
module Game.StartFunctions where
import Game.RoleFunctions
import Core.DbFunctions
import Utils.Utils

import System.Random
import Data.List (permutations)
import Control.Monad (zipWithM_)
import GHC.Generics (Generic)  

import qualified Data.ByteString.Char8 as BS2

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.Types (Query(Query))
import Database.PostgreSQL.Simple.ToField (toField)



-- UserGame Data Type 
data UserGameData = UserGameData{
    player_uuid     :: String, 
    role_idx        :: Int, 
    is_alive        :: Bool, 
    votes           :: Int, 
    kill_vote       :: Int, 
    is_paralized    :: Int, 
    is_silenced     :: Int, 
    is_dead_by_cursed_word :: Bool
}deriving (Show, Generic)

-- Convert UserGameData into a tuple for SQL insert
instance ToRow UserGameData where
    toRow userGameData = [ 
        toField (player_uuid    userGameData), 
        toField (role_idx       userGameData), 
        toField (is_alive       userGameData), 
        toField (votes          userGameData), 
        toField (kill_vote      userGameData), 
        toField (is_paralized   userGameData),
        toField (is_silenced    userGameData),
        toField (is_dead_by_cursed_word userGameData)]


-- Build up a Room
setRoomUpState :: String -> Bool -> IO ()
setRoomUpState rName state = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE Room SET is_up = ? WHERE room_name = ?"
    _ <- execute conn sqlQuery (state, rName)
    ----------------------------------------------
    close conn


-- Set the Role of a Player
setRole :: String -> Int -> IO ()
setRole pUUID roleIdx = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE UserGameData SET role_idx = ? WHERE player_uuid = ?"
    _ <- execute conn sqlQuery (roleIdx, pUUID)
    ----------------------------------------------

    pName       <- getPlayerNameFromUUID pUUID
    roleName    <- getRoleName roleIdx
    putStrLn $ ("> [" ++ (pName) ++ "] User - settado para [" ++ (show roleIdx) ++ "] [" ++ (roleName) ++ "]")

    close conn


-- Random List of Integers
randomList :: Int -> IO [Int]
randomList n = do
    gen <- newStdGen
    let nums = take n $ permutations [1..n]
        (index, _) = randomR (0, length nums - 1) gen
    return $ nums !! index


addPlayersToGame :: String -> IO ()
addPlayersToGame rName = do
    conn            <- getDbConnection
    roomPlayers     <- getRoomPlayersUUIDList rName

    -- Iterate over roomPlayers and perform insertion for each player
    mapM_ (\playerUuid -> do
        let newUserGameData = UserGameData {
            player_uuid     = playerUuid,
            role_idx        = (-1),
            is_alive        = True,
            votes           = 0,
            kill_vote       = 0,
            is_paralized    = 0,
            is_silenced     = 0,
            is_dead_by_cursed_word = False
        }

        -- DB Query ----------------------------------
        let sqlQuery = Query $ BS2.pack "INSERT INTO UserGameData (player_uuid, role_idx, is_alive, votes, kill_vote, is_paralized, is_silenced, is_dead_by_cursed_word) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        execute conn sqlQuery (toRow newUserGameData)
        ----------------------------------------------
        ) roomPlayers
    close conn

    putStrLn $ ("> [" ++ rName ++ "] Room - Todos os jogadores estão no jogo")


-- Distribute Roles to the players
distributeRoles :: String -> IO ()
distributeRoles rName = do 
    let action = "[" ++ (rName) ++ "] [Roles distribution]"  ++ replicate 10 '-'
    putStrLn  action
    
    roles_index  <- randomList 12
    room_players <- getRoomPlayersUUIDList rName

    zipWithM_ (\player roleIndex -> setRole player roleIndex) room_players roles_index

    putStrLn $ replicate (length action) '-'


distributePlayersInitialKnowledge :: String -> IO ()
distributePlayersInitialKnowledge rName = do

    -- Get Role Index
    assassinoIdx    <- getRoleIdx "assassino"
    aprendizIdx     <- getRoleIdx "aprendiz"
    silenciadorIdx  <- getRoleIdx "silenciador"
    juizIdx         <- getRoleIdx "juiz"
    policialIdx     <- getRoleIdx "policial"
    medicoIdx       <- getRoleIdx "medico"

    -- Get Player UUID
    assassino   <- getPlayerUUIDFromRoleIdx assassinoIdx
    aprendiz    <- getPlayerUUIDFromRoleIdx aprendizIdx
    silenciador <- getPlayerUUIDFromRoleIdx silenciadorIdx
    juiz        <- getPlayerUUIDFromRoleIdx juizIdx
    policial    <- getPlayerUUIDFromRoleIdx policialIdx
    medico      <- getPlayerUUIDFromRoleIdx medicoIdx

    -- Set the initial Knowledge
    revealPlayerRole assassino aprendiz
    revealPlayerRole assassino silenciador

    revealPlayerRole aprendiz assassino
    revealPlayerRole aprendiz silenciador

    revealPlayerRole silenciador assassino
    revealPlayerRole silenciador juiz 

    revealPlayerRole juiz medico
    revealPlayerRole policial juiz


-- Check if the player is the room master
isRoomMaster :: String -> String -> IO Bool
isRoomMaster rName pName = do
    conn <- getDbConnection
    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "SELECT room_master FROM Room WHERE room_name = ?"
    [Only result] <- query conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
    return $ result == pName


-- Reset the Round Messages
resetRoundMessages :: String -> IO ()
resetRoundMessages rName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    let sqlQuery = Query $ BS2.pack "UPDATE Room SET round_messages = ARRAY[]::varchar[] WHERE room_name = ?"
    _ <- execute conn sqlQuery (Only rName)
    ----------------------------------------------
    close conn
