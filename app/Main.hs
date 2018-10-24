{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map                      as M
import           Control.Monad.State
import           Text.ParserCombinators.Parsec
import qualified Control.Exception             as E

run :: StateT (M.Map String Type) IO ()
run = do
    input <- lift getLine
    case parse inputParser "<input>" input of
        Right (ICode (code, expect)) -> do
            m <- get
            case mapM (\x -> (x, ) <$> m M.!? x) code of
                Just code -> do
                    let res = evalStateT (inference expect) code
                    (lift . putStrLn) $ maybeAstToString res
                Nothing -> (lift . putStrLn) "定義されてない値があります"
            return ()
        Right (IDefine (name, t)) -> modify $ M.insert name t
        Right (ILoad   file     ) -> do
            input <-
                ( lift
                    . (`E.catch` (\(E.SomeException _) -> return Nothing))
                    . fmap Just
                    . readFile
                    )
                    file
            case input of
                Just input -> case parse varDefineFileParser file input of
                    Right x -> do
                        m <- get
                        put $ foldr (uncurry M.insert) m x
                    Left e -> (lift . print) e
                Nothing -> (lift . putStrLn) "ファイル読み込みエラーが発生しました"
            return ()
        Left e -> (lift . print) e
    run

main :: IO ()
main = do
    runStateT run M.empty
    return ()

nameParser :: Parser String
nameParser = (:) <$> lower <*> many (letter <|> digit)

annotationParser :: Parser Type
annotationParser = string "::" *> typeParser

varDefineParser :: Parser (String, Type)
varDefineParser = (,) <$> nameParser <*> annotationParser

varDefineFileParser :: Parser [(String, Type)]
varDefineFileParser = sepBy varDefineParser newline <* eof

typeFactorParser :: Parser Type
typeFactorParser = char '(' *> typeParser <* char ')' <|> (TValue <$ char '*')

typeParser :: Parser Type
typeParser =
    try (TFunc <$> typeFactorParser <* string "->" <*> typeParser)
        <|> typeFactorParser

codeParser :: Parser ([String], Type)
codeParser = (,) <$> (sepBy nameParser space) <*> annotationParser

loadParser :: Parser String
loadParser = many anyChar

inputParser :: Parser UserInput
inputParser =
    ILoad
        <$> ((try . string) ":l" *> space *> loadParser <* eof)
        <|> IDefine
        <$> ((try . string) ":d" *> space *> varDefineParser <* eof)
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
