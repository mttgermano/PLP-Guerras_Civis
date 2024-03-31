{-# LANGUAGE OverloadedStrings #-}
module Core.BackendServer where

import LoginUtils.PlayerFunctions
import LoginUtils.RoomFunctions
import Controllers.GameController
import Controllers.ApiController

import Web.Scotty
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), decode, object)
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy(..))
import Network.HTTP.Types (status400, status200)

-- Player -------------------------------------------------------
data PlayerJson = PlayerJson { 
    pjName      :: String,
    pjPassword  :: String
} deriving (Show)

instance FromJSON PlayerJson where
    parseJSON = withObject "player" $ \v ->
        PlayerJson <$> v .: "pName" <*> v .: "pPassword"

instance ToJSON PlayerJson where
    toJSON (PlayerJson name password) =
        object ["pName" .= name, "pPassword" .= password]

-- Room ---------------------------------------------------------
data RoomJson = RoomJson{ 
    rpjName     :: String,
    rjName      :: String
} deriving (Show)

instance FromJSON RoomJson where
    parseJSON = withObject "room" $ \v ->
        RoomJson <$> v .: "pName" <*> v .: "rName"

instance ToJSON RoomJson where
    toJSON (RoomJson player_name room_name ) =
        object ["pName" .= player_name, "rName" .= room_name]

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


-- Game Chat --------------------------------------------------
data MessageJson = MessageJson{
    pmName      :: String,
    message     :: String
} deriving (Show)

instance FromJSON MessageJson where
    parseJSON = withObject "message" $ \v ->
        MessageJson <$> v .:"pmName" <*> v .:"message"

instance ToJSON MessageJson where
    toJSON (MessageJson pmName message) =
        object ["pmName" .= pmName, "message" .= message]


-- Main ---------------------------------------------------------
main :: IO ()
main = do
    putStrLn "--> Backend Server on port 3000"
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
                    -- Call createplayer from LoginFunctions
                    result <- liftIO $ createPlayer (pjName playerObj) (pjPassword playerObj)   -- cast Text to String
                    case result of
                        PlayerCreated       pData -> do
                            let pInfo = map (\(Player isBot pId pName pPassword currentRoom) -> object ["isBot" .= isBot, "pUUID" .= pId, "pName" .= pName, "currentRoom" .= currentRoom]) pData

                            status status200
                            json (head pInfo)

                        PlayerAlreadyExist  errMsg  -> do
                            status status400
                            json $ object ["error" .= (errMsg :: String)]

                _ -> do
                    status status400 -- Set HTTP status code to 400 (Bad Request)
                    json $ object ["error" .= ("Invalid player JSON" :: String)]
        
        post "/login/login_player/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (playerObj :: PlayerJson) -> do
                    -- Call loginplayer from LoginFunctions
                    result <- liftIO $ loginPlayer (pjName playerObj) (pjPassword playerObj)
                    case result of
                        PlayerLoggedIn      pData   -> do
                            let pInfo = map (\(Player isBot pId pName pPassword currentRoom) -> object ["isBot" .= isBot, "pUUID" .= pId, "pName" .= pName, "currentRoom" .= currentRoom]) pData

                            status status200
                            json (head pInfo)

                        IncorrectPlayerData errMsg  -> do
                            status status400
                            json $ object ["error" .= (errMsg :: String)]

                _ -> do
                    status status400 -- Set HTTP status code to 400 (Bad Request)
                    json $ object ["error" .= ("Invalid player JSON" :: String)]
                
        -- ROOM PAGE ------------------------------------
        post "/room/create_room/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (roomObj :: RoomJson) -> do
                    -- Call createRoom from LoginFunctions
                    result <- liftIO $ createRoom (rpjName roomObj) (rjName roomObj)
                    case result of
                        RoomCreated               -> return ()
                        RoomAlreadyExist  errMsg  -> do
                            status status400
                            json $ object ["error" .= (errMsg :: String)]

                _ -> do
                    status status400 -- Set HTTP status code to 400 (Bad Request)
                    json $ object ["error" .= ("Invalid room JSON" :: String)]
        
        post "/room/login_room/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (roomObj :: RoomJson) -> do
                    -- Call createRoom from LoginFunctions
                    result <- liftIO $ loginRoom (rpjName roomObj) (rjName roomObj)
                    case result of
                        RoomLoggedIn                -> return ()
                        IncorrectRoomData errMsg    -> do
                            status status400
                            json $ object ["error" .= (errMsg :: String)]

                _ -> do
                    status status400 -- Set HTTP status code to 400 (Bad Request)
                    json $ object ["error" .= ("Invalid room JSON" :: String)]


        -- Game PAGE --------------------------------------
        post "/game/start/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (gameObj :: GameJson) -> do
                    -- Call createRoom from LoginFunctions
                    result <- liftIO $ startGame (grjName gameObj) (gpjName gameObj)
                    case result of
                        GameStarted             -> return ()
                        NotRoomMaster errMsg    -> do  
                            status status400
                            json $ object ["error" .= (errMsg :: String)]
                        RoomAlreadyUp errMsg    -> do
                            status status400
                            json $ object ["error" .= (errMsg :: String)]
                        
                _ -> do
                    status status400 -- Set HTTP status code to 400 (Bad Request)
                    json $ object ["error" .= ("Invalid game JSON" :: String)]


        post "/game/running/action/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (actionObj :: ActionJson) -> do
                    -- Call createRoom from LoginFunctions
                    liftIO $ makeAction (paName actionObj) (action actionObj) (aReciever actionObj)
                _ -> do
                    status status400 -- Set HTTP status code to 400 (Bad Request)
                    json $ object ["error" .= ("Invalid game action JSON" :: String)]


        post "/game/running/send_message/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            requestBody <- body

            case decode requestBody of
                Just (messageObj :: MessageJson) -> do
                    -- Call createRoom from LoginFunctions
                    liftIO $ makeMessage (pmName messageObj) (message messageObj)
                _ -> do
                    status status400 -- Set HTTP status code to 400 (Bad Request)
                    json $ object ["error" .= ("Invalid game message JSON" :: String)]


        -- Front API --------------------------------------
        get "/api/get_room/:rName" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            liftIO $ putStrLn "> Api request for room info"
            
            apiRName <- param "rName"
            -- Call getRoomData from 
            roomData <- liftIO $ getRoomData apiRName
            let roomInfo = map (\(RoomData rName rMaster isUp) -> object ["rName" .= rName, "rMaster" .= rMaster, "isUp" .= isUp]) roomData
            
            status status200
            json (head roomInfo)

        get "/api/get_rooms_list/" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            liftIO $ putStrLn "> Api request for room list"

            rList <- liftIO getRoomList

            status status200
            json $ object ["rList" .= (rList :: [(String, Int)])]

        get "/api/get_room_players/:rName" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            liftIO $ putStrLn "> Api request for room_players"
            apiRName <- param "rName"    

            -- Call createRoom from LoginFunctions
            rPlayers <- liftIO $ getRoomPlayers apiRName
            
            status status200
            json $ object ["rPlayers" .= (rPlayers:: [(String, String, Bool)])]

        get "/api/get_room_state/:rName" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            liftIO $ putStrLn "> Api request for room state"
            apiRName <- param "rName"

            roomData <- liftIO $ getRoomState apiRName
            
            status status200
            json $ object ["rState" .= (roomData :: String)] 


        get "/api/get_player_knowledge/:pName" $ do
            liftIO $ putStrLn $ replicate 50 '-'
            liftIO $ putStrLn "> Api request for player knowledge"
            pName <- param "pName"

            knowledgeData <- liftIO $ getPlayerKnowledge pName 

            let (players, roles) = foldr (\(KnowledgeData pList roleList) (accPlayers, accRoles) -> (pList ++ accPlayers, roleList ++ accRoles)) ([], []) knowledgeData
            let knowledgeInfo = object ["player" .= (players :: [String]), "role" .= (roles :: [String])]

            status status200
            json knowledgeInfo


        -- get "/api/get_actions_results/:rName" $ do
        --     liftIO $ putStrLn $ replicate 50 '-'
        --     liftIO $ putStrLn "> Api request for actions results"
        --     rName <- param "rName"

        --     resultsData <- liftIO $ getRoomActionsResults rName

        --     let (players, actions) = foldr (\(ActionsData pList roleList) (accPlayers, accActions) -> (pList ++ accPlayers, roleList ++ accActions)) ([], []) resultsData
        --     let resultsInfo = object ["player" .= (players :: [String]), "actions" .= (actions :: [String])]

        --     status status200
        --     json resultsInfo
