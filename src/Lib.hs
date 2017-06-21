{-|
Module      : Lib
Description : Lib's main module


-}

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lib
    ( startBot 
    ) where

import Data.Char
import qualified Data.Text as T 
import Pipes
import Text.Parsec 
import Text.Parsec.Text (Parser)
import Network.Discord
import Lib.Prelude hiding (many)
import System.Random

startBot :: IO ()
startBot = do
    token <- T.strip <$> readFile "./token"
    print token
    runBot (Bot $ toS token) $ do
        with ReadyEvent $ handleReady    
        with MessageCreateEvent $ handleMessage

handleReady (Init v u _ _ _) = liftIO . putText $ 
    "Connected to gateway v" <> show v <> " as user " <> show u

handleMessage msg@Message{..} = do
    command msg "!vapor" $ \body -> do
        reply msg (T.map vapor body)
    command msg "!roll"  $ \body -> do
        gen <- liftIO newStdGen
        case parse parseDice "" body of
            Left err -> print err
            Right (count, size) -> 
                let res = prettyList (rolls gen count size) 
                in  reply msg $ mention messageAuthor <> " " <> res 
    command msg "!b" $ \body -> do
        reply msg $ regionalIndicator body

regionalIndicator :: Text -> Text
regionalIndicator = T.concatMap regionize . T.filter isAlpha
    where regionize ch = 
              ":regional_indicator_" <> T.singleton (toLower ch) <> ":" 

mention :: User -> Text
mention u = "<@" <> show (userId u) <> ">"
 
command :: Message -> Text -> (Text -> Effect DiscordM ()) -> Effect DiscordM ()
command Message{..} cmd action =
    case parse (parseCommand $ toS cmd) "" messageContent of
        Left err -> return ()
        Right body -> action body

parseCommand :: [Char] -> Parser Text
parseCommand cmd = do
    void $ string cmd
    spaces
    body <- many (satisfy (const True))
    return (toS body)

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = 
    fetch' $ CreateMessage chan cont Nothing

rolls :: StdGen -> Int -> Int -> [Int]
rolls gen n size = take n $ randomRs (1, size) gen

prettyList :: [Int] -> Text
prettyList [] = "ROLLED NO DICE"
prettyList [n] = "ROLLED " <> show n
prettyList ns = 
    "ROLLED " <> show (sum ns) <> " = " <> (T.intercalate " + " $ fmap show ns)

parseDice :: Parser (Int, Int)
parseDice = do
    count <- digitToInt <$> digit
    void $ char 'd'
    size <- read <$> many1 digit
    spaces
    return (count, size)
        where read = maybe 0 fst . head . reads

-- | Convert printable ascii characters (except space) into corresponding
-- fullwidth characters, by adding an offset of 65248 to their unicode
-- codepoint. Space is mapped to U+3000 IDEOGRAPHIC SPACE instead.
vapor :: Char -> Char
vapor c 
    | ord c == 32                = '\12288'
    | ord c > 32 && ord c <= 126 = chr (ord c + 65248)
    | otherwise                  = c
