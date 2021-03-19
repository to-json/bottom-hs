module Main where

import Data.Char
import System.Environment
import System.Exit
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Bottom

main :: IO()
main = getArgs >>= parse

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse ["-d"] = T.interact bottomDecode
parse []     = T.interact bottomText

usage   = putStrLn "Usage: bottom [-vhd]"
version = putStrLn "Haskell tac 0.1"
exit    = exitWith ExitSuccess
