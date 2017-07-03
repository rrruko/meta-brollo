module Main where

import Protolude
import Bot (startBot)

main :: IO ()
main = forever startBot
