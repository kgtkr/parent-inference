module Main where

import           Data.Map                      as M

main :: IO ()
main = do
    let expect = TValue
    let x      = ("x", TValue)
    let f      = ("f", TFunc TValue TValue)
    let g = ("g", TFunc TValue (TFunc TValue TValue))
    let input  = [f, f, x]
    let res    = inference expect input
    putStrLn $ maybeAstToString res
    return ()

data Type=TValue|TFunc Type Type deriving (Eq,Show)

data AST=AValue String|ACall AST AST deriving (Eq,Show)

maybeAstToString :: Maybe AST -> String
maybeAstToString (Just ast) = astToString ast
maybeAstToString Nothing    = "Error"

astToString :: AST -> String
astToString (AValue name) = name
astToString (ACall a b  ) = "(" ++ astToString a ++ " " ++ astToString b ++ ")"

inferenceFunc :: Type -> AST -> Type -> Type -> [(String, Type)] -> Maybe AST
inferenceFunc expect ast param result _ | expect == TFunc param result =
    Just ast
inferenceFunc expect ast param result xs = do
    p <- inference param xs
    let newAST = ACall ast p
    if expect == result
        then Just newAST
        else case result of
            TFunc a b -> inferenceFunc expect newAST a b xs
            TValue    -> Nothing

inference :: Type -> [(String, Type)] -> Maybe AST
inference TValue      ((name, TValue) : xs) = (Just . AValue) name
inference (TFunc _ _) ((_   , TValue) : _ ) = Nothing
inference expect ((name, TFunc a b) : xs) =
    inferenceFunc expect (AValue name) a b xs
inference _ [] = Nothing
