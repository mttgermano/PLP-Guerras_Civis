module ChatFunctions where

import Control.Concurrent (forkIO)
import Network.Socket
import System.IO
import Control.Exception
import Control.Monad


-- TODO: Ajeitar a Logica
-- == Logica Atual ==
-- Pela logica atual, todas as salas estao utilizando o mesmo TCP server
--
-- == Logica Correta ==
-- 1. Recebo a request da msg
--      {"user": "pedro"; "message": "teste"}
-- 2. Checo em qual sala esse user esta
-- 3. Adiciono a sala ao request
--      {"user": "pedro"; "message": "teste"; "room_name":"sala404"}
-- 4. O user so recebe o broadcast de mensagens que estao na sua respectiva sala


withChatServer :: [Handle] -> IO ()
withChatServer handles = withSocketsDo $ do
    -- Create a TCP socket
    addr <- resolve "3000"      -- listen the request at a port
    sock <- open addr

    -- Listen for incoming connections
    listen sock 12              -- max Number of online players at the chat
    putStrLn "Server listening on port 3000..."

    -- Start accepting connections
    acceptConnections sock handles


resolve :: String -> IO AddrInfo
resolve port = do
    let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
    addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
    return addr


open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    return sock


acceptConnections :: Socket -> [Handle] -> IO ()
acceptConnections sock handles = do
    (conn, _) <- accept sock
    putStrLn "Client connected."

    hdl <- socketToHandle conn ReadWriteMode
    hSetBuffering hdl LineBuffering
    forkIO $ handleClient conn (hdl : handles)

    acceptConnections sock (hdl : handles)


handleClient :: Socket -> [Handle] -> IO ()
handleClient conn handles = do
    hdl <- socketToHandle conn ReadWriteMode
    hSetBuffering hdl LineBuffering

    -- Handle messages from client
    handle (\(SomeException _) -> return ()) $ do
        message <- hGetLine hdl
        putStrLn $ "Received message: " ++ message

        -- Broadcast message to all clients
        broadcast message hdl handles

    hClose hdl
    putStrLn "Client disconnected."


broadcast :: String -> Handle -> [Handle] -> IO ()
broadcast message sender handles = do
    putStrLn $ "Broadcasting message: " ++ message

    mapM_ (\hdl -> when (hdl /= sender) $ hPutStrLn hdl message) handles
