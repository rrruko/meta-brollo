{-# LANGUAGE TypeOperators, FlexibleInstances, DataKinds, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bot
    ( startBot
    ) where

import Lib.Prelude hiding ()
import Lib (parseCommand)
import Language.Haskell.Interpreter
import Data.Char
import Data.List (unlines)
import Data.Proxy
import qualified Data.Text as T
import Network.Discord
import GHC.TypeLits
import System.Random
import Data.Time.Clock
import Data.Time.Format
import Text.Parsec hiding (Reply, (<|>), count)
import Text.Parsec.Text (Parser)

instance DiscordAuth IO where
    auth = Bot . toS . T.strip <$> readFile "./token"
    version = pure "0.2.2"
    runIO = identity

data Command

instance EventMap Command (DiscordApp IO) where
    type Domain   Command = Message
    type Codomain Command = Message

    mapEvent p (m@Message{..})
        | userIsBot messageAuthor = liftIO (putText "Ignoring bot message") *> mzero
        | messageContent == "!roll" = liftIO (putText "Command match") *> pure m
        | otherwise   = liftIO (putText $ "No command match")   *> mzero
    mapEvent p ev = do
        liftIO $ print ev
        mzero

data Reply

instance EventMap Reply (DiscordApp IO) where
    type Domain   Reply = Message
    type Codomain Reply = ()

    mapEvent p msg = do
        command msg "!roll" $ \body -> do
            r <- liftIO $ randomRIO (0 :: Float,1)
            reply msg (show r)
        pure ()

reply :: Message -> Text -> DiscordApp IO ()
reply Message{messageChannel=chan} cont =
    let inRange = T.length cont > 0 && T.length cont <= 2000
    in  when inRange . void . doFetch $ CreateMessage chan cont Nothing

command :: Message -> Text -> (Text -> DiscordApp IO ()) -> DiscordApp IO ()
command Message{..} cmd action =
    let res = parse (parseCommand $ toS cmd) "" messageContent
    in  either (void . pure) action res

data LogEvent a

instance Show a => EventMap (LogEvent a) (DiscordApp IO) where
    type Domain   (LogEvent a) = a
    type Codomain (LogEvent a) = ()

    mapEvent _ e = do
        liftIO $ putText "Logging event"
        liftIO . putText $ show e

type PingPongApp =
    (
        (MessageCreateEvent :<>: MessageUpdateEvent) :>
        (Command :> Reply)
    ) :<>: LogEvent Event

instance EventHandler PingPongApp IO

startBot :: IO ()
startBot = runBot (Proxy :: Proxy (IO PingPongApp))
