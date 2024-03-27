module GameUtils.GameChatFunctions where

import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy.Char8 as BS2
import Network.Socket.ByteString.Lazy (recv)

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), decode, encode, object)
import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll)
import System.IO




sendMessage :: String -> String -> String -> IO ()
sendMessage chaType player msg = do
    -- send a request to the react server    
    putStrLn $ "> Mensagem [" ++ msg ++ "] enviada no canal [" ++ chaType ++ "] do Room [" ++ "rName" ++ "]"
