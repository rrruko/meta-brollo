module Main where

import Protolude
import Lib (startBot)
import Network.WebSockets (ConnectionException(..))

main :: IO ()
main = catch startBot $ \e -> do
    let CloseRequest code _ = e
    putText $ "Got CloseRequest: " <> show e
    when (code == 1001) main
