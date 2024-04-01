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

