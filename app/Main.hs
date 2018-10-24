module Main where

import           Data.Map                      as M
import           Control.Monad.State
import           Text.ParserCombinators.Parsec

getUserInput :: IO String
getUserInput = getUserInput' ""
  where
    getUserInput' s = do
        line <- getLine
        if line == "" then return s else getUserInput' (s ++ line ++ "\n")

main :: IO ()
main = do
    input <- getUserInput
    let x = parse inputParser "input" input
    case x of
        Right x -> do
            let res = runInference x
            putStrLn $ maybeAstToString res
        Left e -> print e
    return ()

nameParser :: Parser String
nameParser = (:) <$> lower <*> many (letter <|> digit)

annotationParser :: Parser Type
annotationParser = string "::" *> typeParser

varDefineParser :: Parser (String, Type)
varDefineParser = (,) <$> nameParser <*> annotationParser

varDefineListParser :: Parser [(String, Type)]
varDefineListParser = sepBy varDefineParser newline

typeFactorParser :: Parser Type
typeFactorParser = char '(' *> typeParser <* char ')' <|> (TValue <$ char '*')

typeParser :: Parser Type
typeParser =
    try (TFunc <$> typeFactorParser <* string "->" <*> typeParser)
        <|> typeFactorParser
opParser :: Parser ([String], Type)
opParser = (,) <$> (char '>' *> sepBy nameParser space) <*> annotationParser

inputParser :: Parser ([(String, Type)], ([String], Type))
inputParser = (,) <$> varDefineListParser <* newline <*> opParser <* eof

runInference :: ([(String, Type)], ([String], Type)) -> Maybe AST
runInference (def, ops) = undefined

data Type=TValue|TFunc Type Type deriving (Eq,Show)

data AST=AValue String|ACall AST AST deriving (Eq,Show)

type Inferencer=StateT [(String, Type)] Maybe
inferenceNext :: Inferencer (String, Type)
inferenceNext = do
    l <- get
    case l of
        (x : xs) -> do
            put xs
            return x
        [] -> lift Nothing

maybeAstToString :: Maybe AST -> String
maybeAstToString (Just ast) = astToString ast
maybeAstToString Nothing    = "Error"

astToString :: AST -> String
astToString (AValue name) = name
astToString (ACall a b  ) = "(" ++ astToString a ++ " " ++ astToString b ++ ")"

inferenceFunc :: Type -> AST -> Type -> Type -> Inferencer AST
inferenceFunc expect ast param result | expect == TFunc param result =
    return ast
inferenceFunc expect ast param result = do
    p <- inference param
    let newAST = ACall ast p
    if expect == result
        then return newAST
        else case result of
            TFunc a b -> inferenceFunc expect newAST a b
            TValue    -> lift Nothing

inference :: Type -> Inferencer AST
inference expect = do
    next <- inferenceNext
    case next of
        (name, TValue) ->
            if expect == TValue then return (AValue name) else lift Nothing
        (name, TFunc a b) -> inferenceFunc expect (AValue name) a b
