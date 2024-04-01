module Game.Interface where

import qualified Data.ByteString.Char8 as BS2

import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Control.Monad (forM)
import Data.List (maximumBy)
import Data.Ord (comparing)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))

import System.Random
import GHC.Base (IO)

import System.Console.ANSI
import System.IO
import Control.Monad

-- Clear the screen
clear :: IO ()
clear = clearScreen >> setCursorPosition 0 0

-- Draw a horizontal line
drawHorizontalLine :: Int -> IO ()
drawHorizontalLine width = putStrLn $ replicate width '-'

-- Draw a vertical line with text centered
drawVerticalLine :: Int -> String -> Int -> IO ()
drawVerticalLine width text padding = do
    let sidePadding = replicate padding ' '
        remainingSpace = width - padding - length text
    putStrLn $ sidePadding ++ text ++ replicate remainingSpace ' '

-- Draw the interface
drawInterface :: IO ()
drawInterface = do
    clear
    setSGR [SetColor Foreground Vivid White]
    drawHorizontalLine 84
    drawVerticalLine 84 "Guerras Civis" 32
    drawHorizontalLine 84
    replicateM_ 10 $ putStrLn "|                                                                               |"
    drawVerticalLine 84 "[1] Criar Usuario" 14
    drawVerticalLine 84 "[2] Loggar" 14
    drawHorizontalLine 84
    setSGR [Reset]
-- Function to create a user with validation
createUser :: String -> IO ()
createUser nome = do
    putStrLn "Insira a senha:"
    senha1 <- getLine
    putStrLn "Confirme a senha:"
    senha2 <- getLine
    if senha1 == senha2
        then putStrLn $ "Usuário '" ++ nome ++ "' criado com sucesso"
        else do
            putStrLn "Senhas devem conferir, tente novamente."
            createUser nome


-- Function to draw the create user interface
drawCreateUserInterface :: IO ()
drawCreateUserInterface = do
    clear
    setSGR [SetColor Foreground Vivid White]
    drawHorizontalLine 84
    drawVerticalLine 84 "Criar usuario" 32
    drawHorizontalLine 84
    putStrLn "|                                                                               |"
    putStrLn "|                                                                               |"
    putStrLn "|                                                                               |"
    drawVerticalLine 84 "Usuario:" 10
    drawVerticalLine 84 "Senha:" 10
    drawVerticalLine 84 "Confirme a senha:" 10
    drawHorizontalLine 84
    setSGR [Reset]


-- Function to login an user
loginUser :: IO ()
loginUser = do
    drawLoginInterface
    putStrLn "Insira o nome:"
    username <- getLine
    putStrLn "Insira a senha:"
    password <- getLine
    --  So exibe 'Usario criado com sucesso' - falta integrar o login
    putStrLn $ "Logado com'" ++ username ++ "'"

-- Function to draw the login interface
drawLoginInterface :: IO ()
drawLoginInterface = do
    clear
    setSGR [SetColor Foreground Vivid White]
    drawHorizontalLine 84
    drawVerticalLine 84 "Loggar" 32
    drawHorizontalLine 84
    putStrLn "|                                                                               |"
    drawVerticalLine 84 "Usuario:" 10
    drawVerticalLine 84 "Senha:" 10
    drawHorizontalLine 84
    setSGR [Reset]


    -- Function to create a room 
createRoom :: String -> IO ()
createRoom roomName = do
    putStrLn "Insira a senha da sala:"
    roomSenha1 <- getLine
    putStrLn "Confirme a senha da sala:"
    roomSenha2 <- getLine
    if roomSenha1 == roomSenha2
        then putStrLn $ "Sala '" ++ roomName ++ "' criada com sucesso"
        else do
            putStrLn "Senhas devem conferir, tente novamente."
            createRoom roomName

-- Function to draw the create room interface
drawCreateRoomInterface :: IO ()
drawCreateRoomInterface = do
    clear
    setSGR [SetColor Foreground Vivid White]
    drawHorizontalLine 84
    drawVerticalLine 84 "Criar sala" 32
    drawHorizontalLine 84
    putStrLn "|                                                                               |"
    putStrLn "|                                                                               |"
    putStrLn "|                                                                               |"
    drawVerticalLine 84 "Nome da sala:" 10
    drawVerticalLine 84 "Senha da sala:" 10
    drawVerticalLine 84 "Confirme a senha:" 10
    drawHorizontalLine 84
    setSGR [Reset]

-- Function to join a room
joinRoom :: IO ()
joinRoom = do
    drawJoinRoomInterface
    putStrLn "Insira o nome da sala:"
    roomName <- getLine
    putStrLn "Insira a senha da sala:"
    roomPassword <- getLine
    --  Integrar logica de entrar na sala 
    putStrLn $ "Usuario entrou na sala '" ++ roomName ++ "'"

-- draw the join room interface
drawJoinRoomInterface :: IO ()
drawJoinRoomInterface = do
    clear
    setSGR [SetColor Foreground Vivid White]
    drawHorizontalLine 84
    drawVerticalLine 84 "Entrar na sala" 32
    drawHorizontalLine 84
    putStrLn "|                                                                               |"
    drawVerticalLine 84 "Nome da sala:" 10
    drawVerticalLine 84 "Senha da sala:" 10
    drawHorizontalLine 84
    setSGR [Reset]




