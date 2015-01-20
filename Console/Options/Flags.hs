module Console.Options.Flags
    ( parseFlags
    , FlagDesc(..)
    , Flag(..)
    , FlagArgDesc(..)
    , FlagError(..)
    , Nid
    ) where

import Control.Applicative
import Control.Monad
import Data.List

type Nid = Int

-- | How to parse a specific flag
data FlagDesc = FlagDesc
    { flagShort         :: Maybe Char             -- ^ short flag parser 'o'
    , flagLong          :: Maybe String           -- ^ long flag "flag"
    , flagDescription   :: Maybe String           -- ^ description of this flag
    , flagNid           :: Nid                    -- ^ flag number. internal value
    , flagArg           :: FlagArgDesc            -- ^ parser for the argument to an flag
    , flagArgValidate   :: String -> Maybe String -- ^ if the argument doesn't validate, return the error message associated, otherwise Nothing
    }

-- | Whether a flag has an flag, 
data FlagArgDesc =
      FlagArgNone
    | FlagArgOptional
    | FlagArgRequired
    deriving (Show,Eq)

data Matching a = NoMatching | Matching a | MatchingWithArg a String

-- | the state of parsing
data ParseState a = ParseState
    { psFlags  :: ![Flag]      -- ^ in reverse order
    , psUnparsed :: ![String]      -- ^ in reverse order
    , psErrors   :: ![FlagError] -- ^ in reverse order
    }

type Flag = (Nid, Maybe String)

data FlagError = FlagError FlagDesc Int String

parseFlags :: [FlagDesc]
             -> [String]
             -> ([Flag], [String], [FlagError])
parseFlags flagParsers = loop (ParseState [] [] []) [1..]
  where
        loop :: ParseState a -> [Int] -> [String] -> ([Flag], [String], [FlagError])
        loop (ParseState os us ers) _      []     = (reverse os, reverse us, reverse ers)
        loop (ParseState os us ers) (i:is) (a:as) =
            case a of
                '-':'-':[]   -> (reverse os, reverse us ++ as, reverse ers)
                '-':'-':long -> loop (processFlag (findLong long)) is as
                '-':short:[] -> loop (processFlag (findShort short)) is as
                _            -> loop (ParseState os (a:us) ers) is as
          where processFlag NoMatching     = ParseState os (a:us) ers
                processFlag (Matching opt) =
                    case flagArg opt of
                        FlagArgNone     -> ParseState ((flagNid opt, Nothing) : os) us ers
                        FlagArgOptional -> ParseState ((flagNid opt, Nothing) : os) us ers
                        FlagArgRequired ->
                            case as of
                                []     -> let e = mkFlagError opt "required argument missing"
                                           in ParseState os (a:us) (e:ers)
                                (x:xs) ->
                                    case (flagArgValidate opt) x of
                                        Nothing     -> ParseState ((flagNid opt, Just x):os) us ers
                                        Just optErr -> let e = mkFlagError opt ("invalid argument: " ++ optErr)
                                                        in ParseState os us (e:ers)
                processFlag (MatchingWithArg opt arg) =
                    case flagArg opt of
                        FlagArgNone     -> let e = mkFlagError opt "invalid argument, expecting no argument" -- fixme: tell which flag
                                            in ParseState os (a:us) (e:ers)
                        FlagArgOptional ->
                            case (flagArgValidate opt) arg of
                                Nothing     -> ParseState ((flagNid opt, Just arg):os) us ers
                                Just optErr -> let e = mkFlagError opt ("invalid argument: " ++ optErr)
                                                in ParseState os us (e:ers)
                        FlagArgRequired ->
                            case (flagArgValidate opt) arg of
                                Nothing     -> ParseState ((flagNid opt, Just arg):os) us ers
                                Just optErr -> let e = mkFlagError opt ("invalid argument: " ++ optErr)
                                                in ParseState os us (e:ers)

                mkFlagError opt s = FlagError opt i s

        findShort short = findRetArg (flagShortMatch short) flagParsers
        findLong long = findRetArg (flagLongMatch long) flagParsers

        flagShortMatch toMatch opt = maybe NoMatching (\x -> if x == toMatch then Matching opt else NoMatching) $ flagShort opt
        flagLongMatch toMatch opt = maybe NoMatching match $ flagLong opt
          where match optLong
                    | leftPart == optLong && null rightPart           = Matching opt
                    | leftPart == optLong && isPrefixOf "=" rightPart = MatchingWithArg opt (drop 1 rightPart)
                    | otherwise                                       = NoMatching
                  where (leftPart,rightPart) = splitAt (length optLong) toMatch

        findRetArg _ []         = NoMatching
        findRetArg f (opt:opts) =
            case f opt of
                NoMatching -> findRetArg f opts
                r          -> r
