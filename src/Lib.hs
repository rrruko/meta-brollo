{-|
Module      : Lib
Description : Lib's main module


-}

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lib
    ( startBot
    ) where

import Lib.Prelude hiding (many, optional, try)
import Language.Haskell.Interpreter
import Data.Char
import Data.List (unlines)
import qualified Data.Text as T
import Pipes
import Text.Parsec
import Text.Parsec.Text (Parser)
import Network.Discord
import System.Random
import Data.Time.Clock
import Data.Time.Format

startBot :: IO ()
startBot = do
    tok <- T.strip <$> readFile "./token"
    print tok
    runBot (Bot $ toS tok) $ do
        with ReadyEvent $ handleReady
        with MessageCreateEvent $ handleMessage
    time <- getCurrentTime
    print $ "Restarting at " <> formatTime defaultTimeLocale "%T%P UTC" time
    startBot

handleReady :: Init -> Effect DiscordM ()
handleReady (Init v u _ _ _) = liftIO . putText $
    "Connected to gateway v" <> show v <> " as user " <> show u

handleMessage :: Message -> Effect DiscordM ()
handleMessage msg@Message{..} = do
    command msg "!vapor" $ \body -> do
        reply msg (T.map vapor body)
    command msg "!roll"  $ \body -> do
        gen <- liftIO newStdGen
        case parse parseDice "" body of
            Left err -> print err
            Right roll@(_, _, mod') ->
                let res = prettyList (rolls gen roll) mod'
                in  reply msg $ mention messageAuthor <> " " <> res
    command msg "!coin" $ \_ -> do
        coin <- liftIO randomIO
        case coin of
            True  -> reply msg $ mention messageAuthor <> " HEADS"
            False -> reply msg $ mention messageAuthor <> " TAILS"
    command msg "!b" $ \body -> reply msg $ regionalIndicator body
    when (validate messageAuthor) $ do
        command msg "!eval" $ interpret' eval' msg
        command msg "!type" $ interpret' typeOf' msg

interpret' :: (Text -> Interpreter [Char]) -> Message -> Text -> Effect DiscordM ()
interpret' action msg body = do
    let parseRes = parse parseEval "" (toS body)
    case parseRes of
        Right body' -> do
            res <- liftIO $ runInterpreter (action (toS body'))
            reply msg . format . toS $ case res of
                Left err -> showErr err
                Right r -> r
        Left _ -> do
            reply msg $ "PLEASE FORMAT YOUR MESSAGE LIKE THIS:\n" <>
                        "\\`\\`\\`hs\n" <>
                        "[CODE]\n" <>
                        "\\`\\`\\`"

eval' :: Text -> Interpreter [Char]
eval' body = do
 setImportsQ imports
 eval . toS $ body

typeOf' :: Text -> Interpreter [Char]
typeOf' body = do
 setImportsQ imports
 typeOf . toS $ body

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont =
    fetch' $ CreateMessage chan cont Nothing

validate :: User -> Bool
validate author = show (userId author) == T.pack "162951695469510656"
               || show (userId author) == T.pack "231224005149851649"

parseEval :: Parser [Char]
parseEval = do
    void $ string "```hs\n"
    manyTill anyChar (try (string "```"))

imports :: [([Char], Maybe [Char])]
imports =
    [ ("Prelude", Nothing)
    , ("Control.Arrow", Nothing)
    , ("Control.Applicative", Nothing)
    , ("Control.Monad", Nothing)
    , ("Data.Bifunctor", Nothing)
    , ("Data.Char", Nothing)
    , ("Data.Complex", Nothing)
    , ("Data.Either", Nothing)
    , ("Data.Foldable", Nothing)
    , ("Data.Functor", Nothing)
    , ("Data.Function", Nothing)
    , ("Data.List", Nothing)
    , ("Data.Ord", Nothing)
    , ("Data.Ratio", Nothing)
    , ("Numeric", Nothing)
    ]

showErr :: InterpreterError -> [Char]
showErr (WontCompile es) = unlines $ map errMsg es
showErr (UnknownError e) = show e
showErr (NotAllowed e) = show e
showErr (GhcException e) = show e

-- | Magic numbers because discord messages are limited to a length of 2000
-- characters.
format :: Text -> Text
format output
    | T.length output < 1993 = "```\n" <> output <> "```"
    | otherwise = "```\n" <> T.take 1990 output <> "..." <> "```"

-- | Epic meme
regionalIndicator :: Text -> Text
regionalIndicator = T.concatMap regionize . T.filter isAlpha
    where regionize ch =
              ":regional_indicator_" <> T.singleton (toLower ch) <> ": "

mention :: User -> Text
mention u = "<@" <> show (userId u) <> ">"

command :: Message -> Text -> (Text -> Effect DiscordM ()) -> Effect DiscordM ()
command Message{..} cmd action =
    let res = parse (parseCommand $ toS cmd) "" messageContent
    in  either (void . return) action res

parseCommand :: [Char] -> Parser Text
parseCommand cmd = do
    void $ string cmd
    spaces
    body <- many (satisfy (const True))
    return (toS body)

rolls :: StdGen -> (Int, Int, Int) -> [Int]
rolls gen (n, size, _) = take n $ randomRs (1, size) gen

prettyList :: [Int] -> Int -> Text
prettyList [] _ = "ROLLED NO DICE"
prettyList [n] 0 = "ROLLED " <> show n
prettyList ns  0 = T.concat ["ROLLED ", show $ sum ns,
    " (", T.intercalate " + " $ fmap show ns, ")"]
prettyList [n] mod' = T.concat ["ROLLED ", show $ n + mod',
    " (", show n, " + ", show mod', ")"]
prettyList ns  mod' = prettyList (mod':ns) 0

{-
!roll 0dn = ROLLED NO DICE
!roll 1dn = ROLLED N
!roll ndm = ROLLED N (A + ... + Z)
!roll 0dn + c = ROLLED NO DICE
!roll 1dn + c = ROLLED N (A + B)
!roll ndm + c = ROLLED N (A + ... + Z + C)
-}

-- | Dice count is limited to one digit, but size can be arbitrary.
parseDice :: Parser (Int, Int, Int)
parseDice = do
    diceCount <- optionMaybe (digitToInt <$> digit)
    void $ char 'd'
    size <- read <$> many1 digit
    spaces
    modifier' <- optionMaybe modifier
    return (fromMaybe 1 diceCount, size, fromMaybe 0 modifier')

read = maybe 0 fst . head . reads

modifier :: Parser Int
modifier = do
    op <- oneOf "+-"
    spaces
    n <- read <$> many1 digit
    if op == '+' then
        return n
    else
        return (-n)

-- | Convert printable ascii characters (except space) into corresponding
-- fullwidth characters, by adding an offset of 65248 to their unicode
-- codepoint. Space is mapped to U+3000 IDEOGRAPHIC SPACE instead.
vapor :: Char -> Char
vapor c
    | ord c == 32                = '\12288'
    | ord c > 32 && ord c <= 126 = chr (ord c + 65248)
    | otherwise                  = c
