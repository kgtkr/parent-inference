module Main where

import           Data.Map                      as M
import           Control.Monad.State
import           Text.ParserCombinators.Parsec

main :: IO ()
main = do
    let expect = TFunc TValue TValue
    let x      = ("x", TValue)
    let f      = ("f", TFunc TValue TValue)
    let g      = ("g", TFunc TValue (TFunc TValue TValue))
    let h      = ("h", TFunc (TFunc TValue TValue) TValue)
    let input  = [g, g, h, g, x, f, g, x, x]
    let res    = evalStateT (inference expect) input
    putStrLn $ maybeAstToString res
    return ()

nameParser :: Parser String
nameParser = (:) <$> lower <*> many (letter <|> digit)

annotationParser :: Parser Type
annotationParser = string "::" *> typeParser

varDefineParser :: Parser (String, Type)
varDefineParser = (,) <$> nameParser <*> annotationParser

varDefineListParser :: Parser [(String, Type)]
varDefineListParser = sepBy varDefineParser newline

typeParser :: Parser Type
typeParser =
    (TValue <$ char '*')
        <|> TFunc
        <$> typeParser
        <*  string "->"
        <*> typeParser
        <|> char '('
        *>  typeParser
        <*  char ')'

callParser :: Parser ([String], Type)
callParser = (,) <$> sepBy nameParser (char ' ') <*> annotationParser

inputParser :: Parser ([(String, Type)], ([String], Type))
inputParser = (,) <$> varDefineListParser <* newline <*> callParser <* eof

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
