import Console.Options
import Data.Char

data XXX = XXX
    deriving (Show)

main = defaultMain $ do
    programName "program"
    programDescription "this is a description of what this program does"

    optAny <- flag 'a' "any" $ FlagRequired $ \s -> if s == "xxx" then Right XXX else Left "xxx"

    optInt <- flag 'i' "int" $ FlagRequired $ \s -> if all isDigit s then Right (read s :: Int) else Left "not an int"

    command "clone" $ do
        action $ \opts -> do
            putStrLn "clone"

    command "pux" $ do
        action $ \opts -> do
            putStrLn ("pux:" ++ show (getArg opts optInt))
            putStrLn ("pux:" ++ show (getArg opts optAny))

{-
    action $ \args -> do
        --putStrLn $ show args
        let n = unArg args optAny
        putStrLn ("main action: --any " ++ show n)
        return ()
-}

{-
main = defaultMain $ do

    programName "program"
    programDescription "this is a description of what this program does"

    sub "command1" $ do
        optionAll <- require 'a' "-all" argNone
        action $ do
            putStrLn "command 1 has been called"

    sub "command2" $ do
        action $ do
            putStrLn "command 2 has been called"
-}

