{-|
Module      : Lib
Description : Lib's main module


-}

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib
    ( startBot
    ) where

import Lib.Prelude hiding (list, many, optional, try)
import Language.Haskell.Interpreter
import Data.Char
import Data.List (unlines)
import qualified Data.Text as T
import Pipes
import Text.Parsec hiding ((<|>), count)
import Text.Parsec.Text (Parser)
import Network.Discord
import System.Random
import Data.Time.Clock
import Data.Time.Format

data ModType = Total | Each
data Mod = Mod Int ModType

-- Describe what dice will be rolled
data Rolls = Rolls {
    count :: Int,
    size :: Int,
    modifier :: Maybe Mod
}

startBot :: IO ()
startBot = do
    tok <- T.strip <$> readFile "./token"
    print tok
    runBot (Bot $ toS tok) $ do
        with ReadyEvent $ handleReady
        with MessageCreateEvent $ handleMessage
    time <- getCurrentTime
    print $ "Restarting at " <> formatTime defaultTimeLocale "%T%P UTC" time

(</>) :: Text -> Text -> Text
l1 </> l2 = l1 <> "\n" <> l2

handleReady :: Init -> Effect DiscordM ()
handleReady (Init v u _ _ _) = liftIO . putText $
    "Connected to gateway v" <> show v <> " as user " <> show u

handleMessage :: Message -> Effect DiscordM ()
handleMessage msg@Message{..} = do
    command msg "!help" $ \body -> reply msg $ getHelp body
    command msg "!roll"  $ \body -> do
        gen <- liftIO newStdGen
        case parse parseDice "" body of
            Left err -> print err
            Right roll@Rolls{..} ->
                let res = prettyList (doRolls gen roll) modifier
                in  reply msg $ mention messageAuthor <> " ROLLED " <> res
    command msg "!coin" $ \_ -> do
        coin <- liftIO randomIO
        case coin of
            True  -> reply msg $ mention messageAuthor <> " HEADS"
            False -> reply msg $ mention messageAuthor <> " TAILS"
    when (validate messageAuthor) $ do
        command msg "!eval" $ interpret' eval' msg
        command msg "!type" $ interpret' typeOf' msg
    command msg "!b" $ \body -> reply msg $ regionalIndicator body
    command msg "!vapor" $ \body -> reply msg (T.map vapor body)

getHelp :: Text -> Text
getHelp "b" = "```!b STRING => print STRING with meme letters```"
getHelp "vapor" = "```!vapor STRING => print STRING with anime meme letters```"
getHelp "roll" = code $
    "!roll dN       => roll 1 N-sided die" </>
    "!roll MdN      => roll M N-sided dice" </>
    "!roll MdN+MOD  => roll M N-sided dice, adding MOD once" </>
    "!roll MdN++MOD => roll M N-sided dice, adding MOD to each"
getHelp "eval" =
    code "!eval BLOCK => evaluate a markdown haskell code block"
getHelp "type" =
    code "!type BLOCK => get the type of a markdown haskell code block"
getHelp "coin" = code "!coin => flip a coin"
getHelp _ = code $ "SAY `!help [command]` TO GET HELP. " </>
                   "COMMANDS: roll, coin, b, vapor, eval, type"


interpret' :: (Text -> Interpreter [Char]) -> Message -> Text -> Effect DiscordM ()
interpret' action msg body = do
    let parseRes = parse parseEval "" (toS body)
    case parseRes of
        Right body' -> do
            res <- liftIO $ runInterpreter (action (toS body'))
            reply msg . code . toS $ case res of
                Left err -> showErr err
                Right r -> r
        Left _ -> do
            reply msg $ "PLEASE FORMAT YOUR MESSAGE LIKE THIS:" </>
                        "\\`\\`\\`hs" </>
                        "[CODE]" </>
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
    let inRange = T.length cont > 0 && T.length cont <= 2000
    in  when inRange . fetch' $ CreateMessage chan cont Nothing

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
code :: Text -> Text
code str
    | T.length str < 1993 = "```" </> str </> "```"
    | otherwise = "```" </> T.take 1990 str <> "..." </> "```"

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
    in  either (void . pure) action res

parseCommand :: [Char] -> Parser Text
parseCommand cmd = do
    void $ string cmd
    spaces
    body <- many (satisfy (const True))
    pure (toS body)

-- | Shows a list of dice roll results and a modifier as text.
prettyList :: [Int] -> Maybe Mod -> Text
prettyList [] _ = "NO DICE"

prettyList [n] Nothing          = show n
prettyList [n] (Just (Mod m _)) = show n <> " " <> withSign m

prettyList ns Nothing =
    list ns <> " = " <> show (sum ns)
prettyList ns (Just (Mod m Total)) =
    list ns <> " (" <> withSign m <> ") = " <> show (sum ns + m)
prettyList ns (Just (Mod m Each)) =
    listWithMod m ns <> " = " <> (show . sum $ map (+m) ns)

list :: [Int] -> Text
list = T.intercalate ", " . map show

listWithMod :: Int -> [Int] -> Text
listWithMod m = T.intercalate ", " . map (withMod m)

withMod :: Int -> Int -> Text
withMod m n = show n <> " (" <> withSign m <> ")"

withSign :: Int -> Text
withSign n
    | n >= 0 = "+" <> show n
    | otherwise = show n

doRolls :: StdGen -> Rolls -> [Int]
doRolls gen Rolls{..} = take count $ randomRs (1, size) gen

-- | Dice count is limited to one digit, but size can be arbitrary.
parseDice :: Parser Rolls
parseDice = do
    count' <- optionMaybe (digitToInt <$> digit)
    void $ char 'd'
    size' <- read <$> many1 digit
    spaces
    modifier' <- optionMaybe parseMod
    pure $ Rolls (fromMaybe 1 count') size' modifier'

parseMod :: Parser Mod
parseMod = do
    op <- try (string "++") <|> try (string "--") <|> try (string "+") <|> string "-"
    spaces
    n <- read <$> many1 digit
    if op == "+" then
        pure $ Mod n Total
    else if op == "++" then
        pure $ Mod n Each
    else if op == "-" then
        pure $ Mod (-n) Total
    else if op == "--" then
        pure $ Mod (-n) Each
    else
        undefined

read :: [Char] -> Int
read = maybe 0 fst . head . reads

-- | Convert printable ascii characters (except space) into corresponding
-- fullwidth characters, by adding an offset of 65248 to their unicode
-- codepoint. Space is mapped to U+3000 IDEOGRAPHIC SPACE instead.
vapor :: Char -> Char
vapor c
    | ord c == 32                = '\12288'
    | ord c > 32 && ord c <= 126 = chr (ord c + 65248)
    | otherwise                  = c
