{-# LANGUAGE OverloadedStrings #-}

module Main where
import LoginFunctions

import Web.Scotty
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), decode, object)
import Data.Text.Lazy (Text)


-- User --------------------
data User = User 
    {   uName :: Text,
        uPassword :: Text 
    } 
    deriving (Show)

instance FromJSON User where
    parseJSON = withObject "user" $ \v -> User <$> v .: "uName" <*> v .: "uPassword"

instance ToJSON User where
    toJSON (User user password) = object ["uName" .= user, "uPassword" .= password]

-- Room --------------------
data Room = Room
    {
        rName :: Text,
        rPassword :: Text
    }
    deriving (Show)

instance FromJSON Room where
    parseJSON = withObject "room" $ \v -> Room <$> v .: "rName" <*> v .: "rPassword"

instance ToJSON Room where
    toJSON (Room room password) = object ["rName" .= room, "rPassword" .= password]
----------------------------


main :: IO ()
main = do
    putStrLn "--> Server on port 3000..."
    scotty 3000 $ do
        get "/" $ do
            text "Server it's up on port 3000"

        get "/hello/:name" $ do
            name <- param "name"
            text $ "Hello, " <> name <> "!"

        -- LOGIN PAGE ------------------------------------
        post "/login/create_user/" $ do
            requestBody <- body
            case decode requestBody of
                Just (userObj :: User) -> do
                    liftIO $ putStrLn $ "Received user: " ++ show userObj
                    -- Call createUser from LoginFunctions
                    liftIO $ createUser requestBody
                    text $ "User created: " <> uName userObj <> ", Password: " <> uPassword userObj
                Nothing -> text "Invalid JSON"
        
        post "/login/login_user/" $ do
            requestBody <- body
            case decode requestBody of
                Just (userObj :: User) -> do
                    liftIO $ putStrLn $ "Received user: " ++ show userObj
                    text $ "User logged: " <> uName userObj <> ", Password: " <> uPassword userObj
                Nothing -> text "Invalid JSON"
        
        -- ROOM PAGE ------------------------------------
        post "/room/create_room/" $ do
            requestBody <- body
            case decode requestBody of
                Just (roomObj :: Room) -> do
                    liftIO $ putStrLn $ "Received room: " ++ show roomObj
                    text $ "Room created: " <> rName roomObj <> ", Password: " <> rPassword roomObj
                Nothing -> text "Invalid JSON"
        
        post "/room/login_room/" $ do
            requestBody <- body
            case decode requestBody of
                Just (roomObj :: Room) -> do
                    liftIO $ putStrLn $ "Received room: " ++ show roomObj
                    text $ "Room logged: " <> rName roomObj <> ", Password: " <> rPassword roomObj
                Nothing -> text "Invalid JSON"

        -- Game PAGE --------------------------------------
