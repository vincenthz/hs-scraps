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
    , description
    , Action
    -- * Arguments
    , FlagParser(..)
    , Arg
    ) where

import Console.Options.Flags

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs, getProgName)
import System.Exit

-- add implementation for windows
hPutErrLn :: String -> IO ()
hPutErrLn = hPutStrLn stderr

data Argument = Argument
    { argumentName        :: String
    , argumentDescription :: String
    , argumentArity       :: Int
    , argumentValidate    :: String -> Maybe String
    }

-- A command that is composed of a hierarchy
--
data Command = Command
    { getCommandHier        :: CommandHier
    , getCommandDescription :: String
    , getCommandOptions     :: [FlagDesc]
    , getCommandAction      :: Action
    }

data CommandHier =
      CommandTree [(String, Command)]
    | CommandLeaf [Argument]

----------------------------------------------------------------------
setDescription desc  (Command hier _ opts act)    = Command hier desc opts act
setAction      ioAct (Command hier desc opts _)   = Command hier desc opts ioAct
addOption      opt   (Command hier desc opts act) = Command hier desc (opt : opts) act
----------------------------------------------------------------------


-- the current state of the program description
-- as the monad unfold ..
data ProgramDesc = ProgramDesc
    { stName        :: Maybe String -- program name
    , stDescription :: Maybe String -- program description
    , stCT          :: Command      -- the command
    , stNextID      :: !Int         -- next id for argument or flag
    }

data Arg a = Arg Int Bool (String -> a)

data FlagParser a =
      FlagRequired (ValueParser a)
    | FlagOptional a (ValueParser a)
    | FlagNothing

type ValueParser a = String -> Either String a

-- use something with faster lookup. using list for now, to not bring dep
data Args = Args [ (Nid, Maybe String) ] -- ^ flag arguments
                 [ (Nid, Maybe String) ] -- ^ unnamed arguments

type Action = (forall a . Arg a -> a) -> IO ()

initialProgramDesc = ProgramDesc Nothing Nothing (Command (CommandLeaf []) "no description" [] noAction) 0
  where noAction :: (forall a . Arg a -> a) -> IO ()
        noAction _ = do
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
        go parsedOpts (Command hier desc opts act) args =
            case parseFlags opts args of
                (opts, unparsed, [])  -> do
                    case hier of
                        CommandTree subs -> do
                            case unparsed of
                                []     -> errorExpectingMode subs
                                (x:xs) -> case lookup x subs of
                                                Nothing      -> errorInvalidMode x subs
                                                Just subTree -> go (opts:parsedOpts) subTree unparsed
                        CommandLeaf unnamedArgs -> do
                            let unnamedOpts = validateUnnamedArgs (reverse unnamedArgs) unparsed
                            act (getArg $ Args (concat (unnamedOpts:opts:parsedOpts)) [])
                (_, _, ers) -> do
                    mapM_ showOptionError ers
                    exitFailure

        validateUnnamedArgs _ _ =
            []

        showOptionError (FlagError opt i s) = do
            let optName = (maybe "" (:[]) $ flagShort opt) ++ " " ++ (maybe "" id $ flagLong opt)
            hPutErrLn ("error: " ++ show i ++ " option " ++ optName ++ " : " ++ s)

        errorExpectingMode subs = do
            mapM_ hPutErrLn $
                [ "error: expecting one of the following mode: "
                , ""
                ] ++ map (indent 4 . fst) subs
            exitFailure
        errorInvalidMode got subs = do
            mapM_ hPutErrLn $
                [ "error: invalid mode '" ++ got ++ "', expecting one of the following mode: "
                , ""
                ] ++ map (indent 4 . fst) subs
            exitFailure

        indent :: Int -> String -> String
        indent n s = replicate n ' ' ++ s

-- | Set the program name
programName :: String -> OptionDesc ()
programName s = modify $ \st -> st { stName = Just s }

-- | Set the program description
programDescription :: String -> OptionDesc ()
programDescription s = modify $ \st -> st { stDescription = Just s }

-- | Set the description for a command
description :: String -> OptionDesc ()
description doc = modify $ \st -> st { stCT = setDescription doc (stCT st) }

modifyHier :: (CommandHier -> CommandHier) -> Command -> Command
modifyHier f (Command hier desc opts act) = Command (f hier) desc opts act

-- | Create a new sub command
command :: String -> OptionDesc () -> OptionDesc ()
command name sub = do
    subSt <- liftIO (gatherDesc sub)
    modify $ \st -> st { stCT = addCommand (stCT subSt) $ stCT st }
  where addCommand subTree = modifyHier $ \hier ->
            case hier of
                CommandLeaf _ -> CommandTree [(name,subTree)]
                CommandTree t -> CommandTree ((name, subTree) : t)

-- | Set the action to run in this command
action :: Action -> OptionDesc ()
action ioAct = modify $ \st -> st { stCT = setAction ioAct (stCT st) }

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

    modify $ \st -> st { stCT = addOption opt (stCT st) }

    case aFcts of
        Nothing     -> return (Arg nid False (\"" -> undefined))
        Just (p, _) -> return (Arg nid False p)
  where (argp, aFcts) = case fp of
                        FlagRequired p   -> (FlagArgHave, Just (toArg p, isValid p))
                        FlagOptional _ p -> (FlagArgMaybe, Just (toArg p, isValid p))
                        FlagNothing      -> (FlagArgNone, Nothing)

        toArg :: (String -> Either String a) -> String -> a
        toArg p = either (error "internal error toArg") id . p

        isValid f = either FlagArgInvalid (const FlagArgValid) . f

-- | An unnamed argument
--
-- For now, argument in a point of tree that contains sub trees will be ignored.
-- TODO: record a warning or add a strict mode (for developping the CLI) and error.
argument :: String -> ValueParser a -> OptionDesc (Arg a)
argument name fp = do
    nid <- getNextID
    let a = Argument { argumentName        = name
                     , argumentDescription = ""
                     , argumentArity       = 1
                     , argumentValidate    = either Just (const Nothing) . fp
                     }
    modify $ \st -> st { stCT = addArg a (stCT st) }
    return (Arg nid True (either (error "internal error") id . fp))
  where addArg arg = modifyHier $ \hier ->
            case hier of
                CommandLeaf l  -> CommandLeaf (arg:l)
                CommandTree {} -> hier -- ignore argument in a hierarchy.

-- | give the ability to set options that are conflicting with each other
-- if option a is given with option b then an conflicting error happens
conflict :: Arg a -> Arg b -> OptionDesc ()
conflict = undefined

{- need a many arguments version and an optional argument
arguments :: OptionDesc (Arg [a])
-}

getArg :: Args -> (forall a . Arg a -> a)
getArg (Args flagArgs _) (Arg nid False p) =
    maybe (error $ "internal error, argument " ++ show nid ++ " not there") (maybe (error "XXX") p) $ lookup nid flagArgs

getArg (Args _ unnamedArgs) (Arg nid True p) =
    error "getArg for unnamed argument not implemented"
