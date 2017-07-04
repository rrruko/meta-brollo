{-# LANGUAGE TypeOperators, FlexibleInstances, DataKinds, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bot
    ( startBot
    ) where

import Lib
import Lib.Prelude
import Interpret (eval', interpret', typeOf', validate)

import qualified Data.Text as T
import Network.Discord
import System.Random
import Text.Parsec (parse)

instance DiscordAuth IO where
    auth = Bot . toS . T.strip <$> readFile "./token"
    version = pure "0.2.2"
    runIO = identity

data Command

instance EventMap Command (DiscordApp IO) where
    type Domain   Command = Message
    type Codomain Command = Message

    mapEvent _ (m@Message{..})
        | messageAuthor == Webhook || userIsBot messageAuthor =
              liftIO (putText "Ignoring bot message") *> mzero
        | otherwise = pure m
        
data Reply

instance EventMap Reply (DiscordApp IO) where
    type Domain   Reply = Message
    type Codomain Reply = ()

    mapEvent _ (msg@Message{..}) = void $ do
        command msg "!roll" $ \body -> do
            gen <- liftIO newStdGen
            case parse parseDice "" body of
                Left err -> print err
                Right roll@Rolls{..} ->
                    let res = prettyList (doRolls gen roll) modifier
                    in  reply msg $ mention messageAuthor <> " " <> res
        command msg "!coin" $ \_ -> do
            r <- liftIO randomIO
            reply msg $ mention messageAuthor <> if r then " HEADS" else " TAILS"
        command msg "!help"  $ \body -> reply msg $ getHelp body
        command msg "!b"     $ \body -> reply msg $ regionalIndicator body
        command msg "!vapor" $ \body -> reply msg $ T.map vapor body
        when (validate messageAuthor) $ do
            command msg "!eval" $ \body -> do
                ans <- liftIO $ interpret' eval' body
                reply msg ans
            command msg "!type" $ \body -> do
                ans <- liftIO $ interpret' typeOf' body
                reply msg ans

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
