{-# LANGUAGE OverloadedStrings #-}
module Main where

import LoginFunctions
import RoomFunctions
import GameController

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
    rpjName :: String,
    rjName :: String,
    rjPassword :: String
} deriving (Show)

instance FromJSON RoomJson where
    parseJSON = withObject "room" $ \v ->
        RoomJson <$> v .: "pName" <*> v .: "rName" <*> v .: "rPassword"

instance ToJSON RoomJson where
    toJSON (RoomJson player_name room_name room_password) =
        object ["pName" .= player_name, "rName" .= room_name, "rPassword" .= room_password]

-- Game ---------------------------------------------------------
data GameJson = GameJson{
    grjName :: String,
    gpjName :: String
} deriving (Show)

instance FromJSON GameJson where
    parseJSON = withObject "game" $ \v ->
        GameJson <$> v .: "rName" <*> v .:"pName"

instance ToJSON GameJson where
    toJSON (GameJson rname pname) =
        object ["rName" .= rname, "pName" .= pname]


-- Main ---------------------------------------------------------
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
                    liftIO $ createRoom (rpjName roomObj) (rjName roomObj) (rjPassword roomObj)   -- cast Text to String
                _ -> text "Invalid room data"
        
        post "/room/login_room/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (roomObj :: RoomJson) -> do
                    liftIO $ putStrLn $ "Room JSON: " ++ show roomObj

                    -- Call createRoom from LoginFunctions
                    liftIO $ loginRoom (rpjName roomObj) (rjName roomObj) (rjPassword roomObj)   -- cast Text to String
                _ -> text "Invalid room data"

        -- Game PAGE --------------------------------------
        post "/game/start/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (gameObj :: GameJson) -> do
                    liftIO $ putStrLn $ "Game JSON: " ++ show gameObj

                    -- Call createRoom from LoginFunctions
                    liftIO $ startGame (grjName gameObj) (gpjName gameObj)   -- cast Text to String
                _ -> text "Invalid game data"

        -- TODO rota post para acabar o game
