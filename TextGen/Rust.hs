module TextGen.Rust
    ( 
    -- * Types
      Param
    -- * Combinators
    , function
    , unsafe
    , def
    ) where

import TextGen.Out
import Data.List (intercalate)

type Param = (String, [String])

semicolon = out ";"

function pre name params body = do
    outNl (pre ++ " fn " ++ name ++ "(" ++ paramOut ++ ") {")
    withIndent $ do
        body
    outNl "}"
  where
    paramOut = intercalate ", " $ map renderParam params
      where renderParam (name, tys) = name ++ ": " ++ intercalate " " tys

unsafe body = out "unsafe { " >> withIndent body >> out "}"

def name body = do
    out ("let " ++ name ++ " = ")
    withIndent $ body >> semicolon
