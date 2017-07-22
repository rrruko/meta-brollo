{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
    ( (</>)
    , Rolls(..)
    , code
    , doRolls
    , getHelp
    , parseCommand
    , parseDice
    , prettyList
    , mention
    , regionalIndicator
    , vapor
    ) where

import Lib.Prelude hiding (list, many, optional, try)

import Data.Char
import qualified Data.Text as T
import Text.Megaparsec hiding ((<|>), count)
import Text.Megaparsec.Text (Parser)
import Network.Discord
import System.Random

data ModType = Total | Each
data Mod = Mod Int ModType

-- Describe what dice will be rolled
data Rolls = Rolls {
    count :: Int,
    size :: Int,
    modifier :: Maybe Mod
}

(</>) :: Text -> Text -> Text
l1 </> l2 = l1 <> "\n" <> l2

getHelp :: Text -> Text
getHelp "b" = code "!b STRING => print STRING with meme letters"
getHelp "vapor" = code "!vapor STRING => print STRING with anime meme letters"
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

parseCommand :: Text -> Parser Text
parseCommand cmd = do
    void . string $ toS cmd
    space
    body <- many . satisfy $ const True
    pure $ toS body

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
    listWithMod m ns <> " = " <> show (sum $ map (+m) ns)

list :: [Int] -> Text
list = T.intercalate ", " . map show

listWithMod :: Int -> [Int] -> Text
listWithMod m = T.intercalate ", " . map (withMod m)

withMod :: Int -> Int -> Text
withMod m n = show n <> " *(" <> withSign m <> ")*"

withSign :: Int -> Text
withSign n
    | n >= 0 = "+" <> show n
    | otherwise = show n

doRolls :: StdGen -> Rolls -> [Int]
doRolls gen Rolls{..} = take count $ randomRs (1, size) gen

-- | Dice count is limited to one digit, but size can be arbitrary.
parseDice :: Parser Rolls
parseDice = do
    howMany <- optional integer 
    void $ char 'd'
    faces <- integer 
    space
    modify <- optional parseMod
    pure $ Rolls (fromMaybe 1 howMany) faces modify

-- | Parses ++, --, +, or -, followed by an integer. Notice the implicit fail.
parseMod :: Parser Mod
parseMod = do
    Just (sign, modType) <- fmap modOperator . many $ oneOf ("+-" :: String)
    space
    n <- integer
    pure $ Mod (sign * n) modType
        where modOperator :: String -> Maybe (Int, ModType)
              modOperator "++" = Just ( 1, Each)
              modOperator "--" = Just (-1, Each)
              modOperator "+"  = Just ( 1, Total)
              modOperator "-"  = Just (-1, Total)
              modOperator _    = Nothing

read :: String -> Int
read = maybe 0 fst . head . reads

integer :: Parser Int
integer = read <$> some digitChar

-- | Convert printable ascii characters (except space) into corresponding
-- fullwidth characters, by adding an offset of 65248 to their unicode
-- codepoint. Space is mapped to U+3000 IDEOGRAPHIC SPACE instead.
vapor :: Char -> Char
vapor c
    | ord c == 32                = '\12288'
    | ord c > 32 && ord c <= 126 = chr (ord c + 65248)
    | otherwise                  = c
