{-# LANGUAGE OverloadedStrings #-}
module Main where

import LoginPlayerFunctions
import LoginRoomFunctions
import GameController

import Web.Scotty
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), decode, object)
import Network.Wai.Middleware.Cors (cors, simpleHeaders, CorsResourcePolicy(..))

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

-- Game Action --------------------------------------------------
data ActionJson = ActionJson{
    paName      :: String,
    action      :: String,
    aReciever   :: String
} deriving (Show)

instance FromJSON ActionJson where
    parseJSON = withObject "action" $ \v ->
        ActionJson <$> v .: "paName" <*> v .:"action" <*> v .:"aReciever"

instance ToJSON ActionJson where
    toJSON (ActionJson pName action aReciever) =
        object ["pName" .= pName, "action" .= action, "aReciever" .= aReciever]


-- Main ---------------------------------------------------------
main :: IO ()
main = do
    putStrLn "--> Server on port 3000..."
    liftIO $ putStrLn $ replicate 50 '-'
    scotty 3000 $ do
        middleware $ cors $ const $ Just $ CorsResourcePolicy {
            corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"],
            corsRequestHeaders = ["Content-Type"],
            corsExposedHeaders = Nothing,
            corsOrigins = Nothing,
            corsMaxAge = Nothing,
            corsVaryOrigin = True,
            corsRequireOrigin = False,
            corsIgnoreFailures = False
        }

        get "/" $ do
            text "> Server it's up on port 3000"

        -- LOGIN PAGE ------------------------------------
        post "/login/create_player/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (playerObj :: PlayerJson) -> do
                    liftIO $ putStrLn $ "JSON [player]: " ++ show playerObj

                    -- Call createplayer from LoginFunctions
                    liftIO $ createPlayer (pjName playerObj) (pjPassword playerObj)   -- cast Text to String
                --_ -> text "Invalid player data"
                _ -> json $ object ["error" .= ("Invalid player data" :: String)]
        
        post "/login/login_player/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (playerObj :: PlayerJson) -> do
                    liftIO $ putStrLn $ "JSON [player]: " ++ show playerObj

                    -- Call loginplayer from LoginFunctions
                    liftIO $ loginPlayer (pjName playerObj) (pjPassword playerObj)   -- cast Text to String
                _ -> text "Invalid player data"
                
        -- ROOM PAGE ------------------------------------
        post "/room/create_room/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (roomObj :: RoomJson) -> do
                    liftIO $ putStrLn $ "JSON [room]: " ++ show roomObj

                    -- Call createRoom from LoginFunctions
                    liftIO $ createRoom (rpjName roomObj) (rjName roomObj) (rjPassword roomObj)   -- cast Text to String
                _ -> text "Invalid room data"
        
        post "/room/login_room/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (roomObj :: RoomJson) -> do
                    liftIO $ putStrLn $ "JSON [room]: " ++ show roomObj

                    -- Call createRoom from LoginFunctions
                    liftIO $ loginRoom (rpjName roomObj) (rjName roomObj) (rjPassword roomObj)   -- cast Text to String
                _ -> text "Invalid room data"

        -- Game PAGE --------------------------------------
        post "/game/start/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (gameObj :: GameJson) -> do
                    liftIO $ putStrLn $ "JSON [game]: " ++ show gameObj

                    -- Call createRoom from LoginFunctions
                    liftIO $ startGame (grjName gameObj) (gpjName gameObj)   -- cast Text to String
                _ -> text "Invalid game data"


        post "/game/running/action/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (actionObj :: ActionJson) -> do
                    liftIO $ putStrLn $ "JSON [game]: " ++ show actionObj

                    -- Call createRoom from LoginFunctions
                    liftIO $ makeAction (paName actionObj) (action actionObj) (aReciever actionObj)  -- cast Text to String
                _ -> text "Invalid game data"

        -- TODO rota post para acabar o game
