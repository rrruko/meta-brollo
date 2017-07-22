module Main where

import Protolude
import Bot (startBot)

main :: IO ()
main = catch startBot $ \e -> do
    print (e :: SomeException)
    main
