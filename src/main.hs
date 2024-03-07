{-# LANGUAGE OverloadedStrings #-}
module Main where

import LoginFunctions
import RoomFunctions

import Web.Scotty
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), (.:?), decode, object)



instance FromJSON Player where
    parseJSON = withObject "player" $ \v ->
        Player <$> v .: "pId" <*> v .: "pName" <*> v .: "pPassword" <*> v .:? "currentRoom"

instance ToJSON Player where
    toJSON (Player playerId playerName playerPassword maybeCurrentRoom) =
        object ["pId" .= playerId, "pName" .= playerName, "pPassword" .= playerPassword, "currentRoom" .= maybeCurrentRoom]

instance FromJSON Room where
    parseJSON = withObject "room" $ \v ->
        Room <$> v .: "rId" <*> v .: "rName" <*> v .: "rPassword"

instance ToJSON Room where
    toJSON (Room roomId roomName roomPassword) =
        object ["rId" .= roomId, "rName" .= roomName, "rPassword" .= roomPassword]


main :: IO ()
main = do
    putStrLn "--> Server on port 3000..."
    scotty 3000 $ do
        get "/" $ do
            text "Server it's up on port 3000"

        -- LOGIN PAGE ------------------------------------
        post "/login/create_player/" $ do
            requestBody <- body
            case decode requestBody of
                Just (playerObj :: Player) -> do
                    liftIO $ putStrLn $ "Received player: " ++ show playerObj

                    -- Call createplayer from LoginFunctions
                    liftIO $ createPlayer (pName playerObj) (pPassword playerObj)   -- cast Text to String
                    liftIO $ putStrLn $ "Player created: " ++ pName playerObj ++ ", Password: " ++ pPassword playerObj
                _ -> text "Invalid player data"
        
        post "/login/login_player/" $ do
            requestBody <- body
            case decode requestBody of
                Just (playerObj :: Player) -> do
                    liftIO $ putStrLn $ "Received player: " ++ show playerObj

                    -- Call loginplayer from LoginFunctions
                    liftIO $ loginPlayer (pName playerObj) (pPassword playerObj)   -- cast Text to String
                    liftIO $ putStrLn $ "Player logged: " ++ pName playerObj ++ ", Password: " ++ pPassword playerObj
                _ -> text "Invalid player data"
        
        -- ROOM PAGE ------------------------------------
        post "/room/create_room/" $ do
            requestBody <- body
            case decode requestBody of
                Just (roomObj :: Room) -> do
                    liftIO $ putStrLn $ "Received room: " ++ show roomObj

                    -- Call createRoom from LoginFunctions
                    liftIO $ createRoom  (rName roomObj) (rPassword roomObj)   -- cast Text to String
                    liftIO $ putStrLn $ "Room created: " ++ rName roomObj ++ ", Password: " ++ rPassword roomObj
                _ -> text "Invalid room data"
        
        post "/room/login_room/" $ do
            requestBody <- body
            case decode requestBody of
                Just (roomObj :: Room) -> do
                    liftIO $ putStrLn $ "Received room: " ++ show roomObj

                    -- Call createRoom from LoginFunctions
                    liftIO $ loginRoom  (rName roomObj) (rPassword roomObj)   -- cast Text to String
                    liftIO $ putStrLn $ "Room logged: " ++ rName roomObj ++ ", Password: " ++ rPassword roomObj
                _ -> text "Invalid room data"

        -- Game PAGE --------------------------------------
