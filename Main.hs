module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson
import UserFunctions

-- Parse a POST request
parsePostRequest :: BS.ByteString -> Maybe User
parsePostRequest = decode

-- Parse a JSON PUT request
parsePutRequest :: BS.ByteString -> Maybe String
parsePutRequest = decode

main :: IO ()
main = do
    -- tests
    let postRequest = "{\"user_name\": \"admin\", \"password\":\"pass123\"}"
    let putRequest = "{\"vote\": \"user2\"}"

    case parsePostRequest (BS.pack postRequest) of
        Just user -> createUser user
        Nothing -> putStrLn "Invalid POST request format"

    case parsePutRequest (BS.pack putRequest) of
        Just userName -> incrementVote userName
        Nothing -> putStrLn "Invalid PUT request format"
