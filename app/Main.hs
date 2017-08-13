module Main where

import Protolude
import Bot (startBot)

main :: IO ()
main = catch startBot $ \e -> do
    putText "*** Exception:"
    putText $ "  " <> show (e :: SomeException)
