module Main where

import           Data.Map                      as M
import           Control.Monad.State
import           Text.ParserCombinators.Parsec

run :: Map String Type -> IO ()
run m = do
    return ()

main :: IO ()
main = do
    {-- input <- getUserInput
    let x = parse inputParser "<input>" input
    case x of
        Right x -> do
            let res = runInference x
            putStrLn $ maybeAstToString res
        Left e -> print e --}
    run M.empty
    return ()

nameParser :: Parser String
nameParser = (:) <$> lower <*> many (letter <|> digit)

annotationParser :: Parser Type
annotationParser = string "::" *> typeParser

varDefineParser :: Parser (String, Type)
varDefineParser = (,) <$> nameParser <*> annotationParser

typeFactorParser :: Parser Type
typeFactorParser = char '(' *> typeParser <* char ')' <|> (TValue <$ char '*')

typeParser :: Parser Type
typeParser =
    try (TFunc <$> typeFactorParser <* string "->" <*> typeParser)
        <|> typeFactorParser

codeParser :: Parser ([String], Type)
codeParser = (,) <$> (char '>' *> sepBy nameParser space) <*> annotationParser

loadParser :: Parser String
loadParser = many anyChar

inputParser :: Parser UserInput
inputParser =
    ILoad
        <$> (string ":d" *> loadParser <* eof)
        <|> IDefine
        <$> (string ":d" *> varDefineParser <* eof)
        <|> ICode
        <$> (codeParser <* eof)

data UserInput=ILoad String|IDefine (String, Type)|ICode ([String], Type)

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
