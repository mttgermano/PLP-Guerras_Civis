module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson
import LoginFunctions


main :: IO ()
main = do

    -- init the TCP server:
    -- withChatServer
 
    -- tests
    let postRequest = "{\"user_name\": \"admin\", \"password\":\"pass123\"}"
    let putRequest = "{\"vote\": \"user2\"}"

    case parsePostRequest (BS.pack postRequest) of
        Just user -> createUser user
        Nothing -> putStrLn "Invalid POST request format"

    case parsePutRequest (BS.pack putRequest) of
        Just userName -> incrementVote userName
        Nothing -> putStrLn "Invalid PUT request format"

    -- todo
    -- test the login user function
    -- test the create room function
    -- test the login room function
    -- test the TCP connection

-- Parse a POST request
parsePostRequest :: BS.ByteString -> Maybe User
parsePostRequest = decode

-- Parse a JSON PUT request
parsePutRequest :: BS.ByteString -> Maybe String
parsePutRequest = decode
