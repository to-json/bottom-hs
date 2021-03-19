module Bottom where

import Data.Char
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as T
import qualified Data.Text.IO as T

bottomText :: T.Text -> T.Text
bottomText x = T.concatMap bottomEncodeChar x

bottomDecode :: T.Text -> T.Text
bottomDecode x = T.pack $ map bottomDecodeChar 
                        $ init 
                        $ T.splitOn bottomSeparator x

-- ----- -- private api below. not bothering with an internal module -- ----- --

bottomSeparator :: T.Text
bottomSeparator = T.pack ['\128073', '\128072']

bottomHeart :: T.Text
bottomHeart = T.pack ['\10084', '\65039']

bottomEncodeChar :: Char -> T.Text
bottomEncodeChar x =
    let y = ord x in
    if y == 0
        then T.concat [bottomHeart, bottomSeparator]
    else T.concat [T.pack (innerBottomEncodeChar y), bottomSeparator]

innerBottomEncodeChar :: Int -> [Char]
innerBottomEncodeChar x
    | x == 0 = []
    | x >= 200 = '\129730' : innerBottomEncodeChar (x - 200)
    | x >= 50 = '\128150' : innerBottomEncodeChar (x - 50)
    | x >= 10 = '\10024' : innerBottomEncodeChar (x - 10)
    | x >= 5 = '\129402' : innerBottomEncodeChar (x - 5)
    | x >= 1 = ',' : innerBottomEncodeChar (x - 1)

bottomDecodeChar :: T.Text -> Char
bottomDecodeChar x =
    if x == bottomHeart
        then chr 0
    else chr $ sum $ map innerBottomDecodeChar $ T.unpack x

innerBottomDecodeChar :: Char -> Int
innerBottomDecodeChar x
    | x == '\129730' = 200
    | x == '\128150' = 50
    | x == '\10024' = 10
    | x == '\129402' = 5
    | x == ',' = 1
    | otherwise = 6942069 -- should not be reached when parsing valid bottomText
