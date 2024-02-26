module GameFunctions where

import LoginFunctions
import Database.PostgreSQL.Simple


-- Increment the vote for a user in the db
incrementVote :: String -> IO ()
incrementVote userName = do
    conn <- getDbConnection

    -- DB Query ----------------------------------
    [Only user_id] <- query conn "SELECT user_id FROM users WHERE user_name = ?" (Only userName)
    execute conn "UPDATE votes SET number_of_votes = number_of_votes + 1 WHERE user_id = ?" (Only (user_id :: Int))
    ----------------------------------------------
    close conn
    putStrLn $ "Vote incremented for user " ++ userName

-- Establish a database connection
getDbConnection :: IO Connection
getDbConnection = connectPostgreSQL "host=localhost dbname=mydatabase user=myuser password=mypassword"
