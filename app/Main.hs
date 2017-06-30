module Main where

import Protolude
import Lib (startBot)
import Network.WebSockets (ConnectionException)

main :: IO ()
main = catch startBot (\e -> do
    print (e :: ConnectionException)
    main)
