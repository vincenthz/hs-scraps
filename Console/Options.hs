{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Console.Options
    (
    -- * Running
      defaultMain
    , defaultMainWith
    -- * Description
    , programName
    , programDescription
    , command
    , flag
    , action
    -- * Arguments
    , FlagParser(..)
    , Arg
    , getArg
    ) where

import Console.Options.Flags

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs, getProgName)
import System.Exit

hPutErrLn :: String -> IO ()
hPutErrLn = hPutStrLn stderr

data Argument = Argument

-- A command that is composed of a hierarchy
--
data Command = Command
    { getCommandHier      :: CommandHier
    , getCommandOptions   :: [FlagDesc]
    , getCommandAction    :: Action
    }

data CommandHier =
      CommandTree [(String, Command)]
    | CommandLeaf [Argument]

-- the current state of the program description
-- as the monad unfold ..
data ProgramDesc = ProgramDesc
    { stName        :: Maybe String -- program name
    , stDescription :: Maybe String -- program description
    , stCT          :: Command      -- the command
    , stNextID      :: !Int         -- next id for argument or flag
    }

-- use something with faster lookup. using list for now, to not bring dep
newtype Args = Args [ (Nid, Maybe String) ]

type Action = Args -> IO ()

initialProgramDesc = ProgramDesc Nothing Nothing (Command (CommandLeaf []) [] noAction) 0
  where noAction _ = do
            hPutErrLn "error: no action defined, using default handler"
            exitFailure

newtype OptionDesc a = OptionDesc { runOptionDesc :: StateT ProgramDesc IO a }
    deriving (Functor,Applicative,Monad,MonadIO,MonadState ProgramDesc)

defaultMain :: OptionDesc () -> IO ()
defaultMain dsl = getArgs >>= defaultMainWith dsl

defaultMainWith :: OptionDesc () -> [String] -> IO ()
defaultMainWith dsl args = do
    descState   <- gatherDesc dsl
    programName <- maybe getProgName return $ stName descState
    let programDescription = maybe "" id $ stDescription descState
    runOptions programName programDescription (stCT descState) args

gatherDesc dsl = execStateT (runOptionDesc dsl) initialProgramDesc

runOptions :: String   -- program name
           -> String   -- program description
           -> Command  -- commands
           -> [String] -- arguments
           -> IO ()
runOptions _ _ ct = go [] ct
  where
        go :: [[Flag]] -> Command -> [String] -> IO ()
        go parsedOpts (Command hier opts act) args =
            case parseFlags opts args of
                (opts, unparsed, [])  -> do
                    case hier of
                        CommandTree subs -> do
                            case unparsed of
                                []     -> error "invalid command line, expecting mode" -- fixme better error
                                (x:xs) -> case lookup x subs of
                                                Nothing      -> error ("unknown mode " ++ show x)
                                                Just subTree -> go (opts:parsedOpts) subTree unparsed
                        CommandLeaf _    -> -- fixme parse arguments
                            act $ Args $ concat (opts:parsedOpts)
                (_, _, ers) -> do
                    mapM_ showOptionError ers
                    exitFailure

        showOptionError (FlagError opt i s) = do
            let optName = (maybe "" (:[]) $ flagShort opt) ++ " " ++ (maybe "" id $ flagLong opt)
            hPutErrLn ("error: " ++ show i ++ " option " ++ optName ++ " : " ++ s)


-- | Set the program name
programName :: String -> OptionDesc ()
programName s = modify $ \st -> st { stName = Just s }

-- | Set the program description
programDescription :: String -> OptionDesc ()
programDescription s = modify $ \st -> st { stDescription = Just s }

-- | Create a new sub command
command :: String -> OptionDesc () -> OptionDesc ()
command name sub = do
    subSt <- liftIO (gatherDesc sub)
    modify $ \st -> st { stCT = addCommand (stCT subSt) $ stCT st }
  where addCommand subTree (Command hier opts act) =
            case hier of
                CommandLeaf _ -> Command (CommandTree [(name,subTree)]) opts act
                CommandTree t -> Command (CommandTree ((name, subTree) : t)) opts act

-- | Set the action to run in this command
action :: Action -> OptionDesc ()
action ioAct = modify $ \st -> st { stCT = setAction (stCT st) }
  where setAction (Command hier opts _) = Command hier opts ioAct

data Arg a = Arg Int (String -> a)

data FlagParser a =
      FlagRequired (FlagArgParser a)
    | FlagOptional a (FlagArgParser a)
    | FlagNothing

type FlagArgParser a = String -> Either String a

--flag 'c' "cuzt" $ FlagRequired intParser

{-
data FlagDesc = FlagDesc

short :: Char -> FlagDesc ()
short c = undefined

long :: String -> FlagDesc ()
long c = undefined

description :: String -> FlagDesc ()
description = undefined
-}

getNextID = do
    nid <- stNextID <$> get
    modify $ \st -> st { stNextID = (stNextID st) + 1 }
    return nid

-- | Flag option either of the form -short or --long
flag :: Char -> String -> FlagParser a -> OptionDesc (Arg a)
flag short long fp = do
    nid <- getNextID

    let opt = FlagDesc
                { flagShort       = Just short
                , flagLong        = Just long
                , flagDescription = Nothing
                , flagNid         = nid
                , flagArg         = argp
                , flagArgValidate = maybe (\_ -> error "internal error: evaluating without argument") snd $ aFcts
                }

    modify $ \st -> st { stCT = addOpt opt (stCT st) }

    case aFcts of
        Nothing     -> return (Arg nid (\"" -> undefined))
        Just (p, _) -> return (Arg nid p)
  where (argp, aFcts) = case fp of
                        FlagRequired p   -> (FlagArgRequired, Just (toArg p, isValid p))
                        FlagOptional _ p -> (FlagArgFlagal, Just (toArg p, isValid p))
                        FlagNothing      -> (FlagArgNone, Nothing)

        toArg :: (String -> Either String a) -> String -> a
        toArg p = either (error "internal error toArg") id . p

        addOpt opt (Command hier opts act) =
            Command hier (opt : opts) act

        isValid :: forall a . (String -> Either String a) -> String -> Maybe String
        isValid f = either Just (const Nothing) . f

-- | give the ability to set options that are conflicting with each other
-- if option a is given with option b then an conflicting error happens
conflict :: Arg a -> Arg b -> OptionDesc ()
conflict = undefined

-- | An unnamed argument
argument :: OptionDesc (Arg a)
argument = undefined

{- need a many arguments version and an optional argument
arguments :: OptionDesc (Arg [a])
-}

getArg :: Args -> Arg a -> a
getArg (Args args) (Arg nid p) =
    maybe (error $ "internal error, argument " ++ show nid ++ " not there") (maybe (error "XXX") p) $ lookup nid args
