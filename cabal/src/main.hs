{-# LANGUAGE OverloadedStrings #-}
module Main where

import LoginFunctions
import RoomFunctions

import Web.Scotty
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), decode, object)



-- Player -------------------------------------------------------
data PlayerJson = PlayerJson { 
    pjName :: String,
    pjPassword :: String
} deriving (Show)

instance FromJSON PlayerJson where
    parseJSON = withObject "player" $ \v ->
        PlayerJson <$> v .: "pName" <*> v .: "pPassword"

instance ToJSON PlayerJson where
    toJSON (PlayerJson name password) =
        object ["pName" .= name, "pPassword" .= password]

-- Room ---------------------------------------------------------
data RoomJson = RoomJson{ 
    rjName :: String,
    rjPassword :: String
} deriving (Show)

instance FromJSON RoomJson where
    parseJSON = withObject "room" $ \v ->
        RoomJson <$> v .: "rName" <*> v .: "rPassword"

instance ToJSON RoomJson where
    toJSON (RoomJson name password) =
        object ["rName" .= name, "rPassword" .= password]


main :: IO ()
main = do
    putStrLn "--> Server on port 3000..."
    liftIO $ putStrLn $ replicate 50 '-'
    scotty 3000 $ do

        get "/" $ do
            text "Server it's up on port 3000"

        -- LOGIN PAGE ------------------------------------
        post "/login/create_player/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (playerObj :: PlayerJson) -> do
                    liftIO $ putStrLn $ "Player JSON: " ++ show playerObj

                    -- Call createplayer from LoginFunctions
                    liftIO $ createPlayer (pjName playerObj) (pjPassword playerObj)   -- cast Text to String
                _ -> text "Invalid player data"
        
        post "/login/login_player/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (playerObj :: PlayerJson) -> do
                    liftIO $ putStrLn $ "Player JSON: " ++ show playerObj

                    -- Call loginplayer from LoginFunctions
                    liftIO $ loginPlayer (pjName playerObj) (pjPassword playerObj)   -- cast Text to String
                _ -> text "Invalid player data"
                
        -- ROOM PAGE ------------------------------------
        post "/room/create_room/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (roomObj :: RoomJson) -> do
                    liftIO $ putStrLn $ "Room JSON: " ++ show roomObj

                    -- Call createRoom from LoginFunctions
                    liftIO $ createRoom  (rjName roomObj) (rjPassword roomObj)   -- cast Text to String
                _ -> text "Invalid room data"
        
        post "/room/login_room/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (roomObj :: RoomJson) -> do
                    liftIO $ putStrLn $ "Room JSON: " ++ show roomObj

                    -- Call createRoom from LoginFunctions
                    liftIO $ loginRoom  (rjName roomObj) (rjPassword roomObj)   -- cast Text to String
                _ -> text "Invalid room data"

        -- Game PAGE --------------------------------------
