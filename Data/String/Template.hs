module Data.String.Template
    ( Template
    , parseTemplate
    , renderTemplate
    ) where

import Data.Char (isDigit, isAlpha)

data TAtom = Text String | Var String deriving (Show)
type Template = [TAtom]

renderTemplate :: Template -> [(String,String)] -> String
renderTemplate template attrs =
    concat $ map renderAtom template
  where
        renderAtom :: TAtom -> String
        renderAtom (Text b) = b
        renderAtom (Var s)  = maybe "" id $ lookup s attrs

parseTemplate :: String -> Template
parseTemplate content 
    | null content        = []
    | head content == '$' = parseVar $ tail content
    | otherwise           = parseText content
  where
        parseText :: String -> Template
        parseText s
            | null s    = []
            | otherwise = Text b : (parseVar $ tailSafe a)
          where
                (b, a) = break ((==) '$') s

        parseVar :: String -> Template
        parseVar s
            | null s    = []
            | otherwise =
                let (b, a) = break ((==) '$') s in
                if isVariable b
                    then Var b  : (parseText $ tailSafe a)
                    else Text b : (parseVar $ tailSafe a)

        isVariable :: String -> Bool
        isVariable = and . map isVariableChar
          where isVariableChar :: Char -> Bool
                isVariableChar c = isAlpha c || isDigit c

        tailSafe s
            | null s    = []
            | otherwise = tail s
