module Main where

import           Data.Map                      as M
import           Control.Monad.State

main :: IO ()
main = do
    let expect = TValue
    let x      = ("x", TValue)
    let f      = ("f", TFunc TValue TValue)
    let g = ("g", TFunc TValue (TFunc TValue TValue))
    let h = ("h", TFunc (TFunc TValue TValue) TValue)
    let input  = [g, g, h, g, x, f, g, x, x]
    let res    = evalStateT (inference expect) input
    putStrLn $ maybeAstToString res
    return ()

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
