module Interpret
    ( eval'
    , interpret'
    , typeOf'
    , validate
    ) where

import Lib
import Lib.Prelude hiding (list, many, optional, try)

import Language.Haskell.Interpreter
import Text.Parsec.Text (Parser)
import Network.Discord

import Data.List (unlines)
import Text.Parsec hiding ((<|>), count)

interpret' :: (Text -> Interpreter String)-> Text -> IO Text
interpret' action body = case parseRes of
    Right body' -> do
        res <- runInterpreter . action $ toS body'
        pure $ code . toS $ case res of
            Left err -> showErr err
            Right r -> r
    Left _ -> pure $
        "PLEASE FORMAT YOUR MESSAGE LIKE THIS:" </>
        "\\`\\`\\`hs" </>
        "[CODE]" </>
        "\\`\\`\\`"
    where parseRes = parse parseEval "" $ toS body

eval' :: Text -> Interpreter String 
eval' body = do
    setImportsQ imports
    eval . toS $ body

typeOf' :: Text -> Interpreter String
typeOf' body = do
    setImportsQ imports
    typeOf . toS $ body

validate :: User -> Bool
validate author = show (userId author) `elem` bigBoys
    where bigBoys :: [Text]
          bigBoys = ["162951695469510656", "231224005149851649"] 

parseEval :: Parser String
parseEval = do
    void $ string "```hs\n"
    manyTill anyChar (try (string "```"))

imports :: [(String, Maybe String)]
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

showErr :: InterpreterError -> String
showErr (WontCompile es) = unlines $ map errMsg es
showErr (UnknownError e) = show e
showErr (NotAllowed e) = show e
showErr (GhcException e) = show e
