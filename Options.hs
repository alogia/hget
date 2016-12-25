module Options where

import System.IO
import System.Console.GetOpt
import System.FilePath.Posix((</>))
import System.Exit

import General
import EncFS
import LispScript

header = "Usage: hget [OPTIONS...] [urls...]"
help = usageInfo header options

data HOptions = HOptions 
    {   verbose       :: Bool,
        optsFile      :: FilePath,
        dataBase      :: FilePath,
        encFSOpts     :: EncFS }
        deriving (Eq, Show)

defaultHOptions = HOptions
    {   verbose     = False,
        optsFile    = "hgetrc",
        dataBase    = (mount defaultEncFS) </> "hgetdb.db",
        encFSOpts   = defaultEncFS }

data Flag = 
      Verbose 
    | Version 
    | Help 
    | EncFSRoot     String
    | EncFSMount    String 
    | EncFSPass     String
    | RCFile        String 
    | DataBase      String
            deriving (Eq,Show)

options :: [OptDescr Flag]
options = [ 
     Option ['v'] ["version"]       (NoArg Version           )  "Show version number" 
   , Option ['V'] ["verbose"]       (NoArg Verbose           )  "Print extra information to terminal" 
   , Option ['h'] ["help"]          (NoArg Help              )  "Show help"
   , Option ['r'] ["encfs-root"]    (ReqArg EncFSRoot  "FILE")  "Set encfs root location" 
   , Option ['d'] ["database"]      (ReqArg DataBase   "File")  "Set database path"
   , Option ['m'] ["encfs-mount"]   (ReqArg EncFSMount "FILE")  "Set encfs mount location" 
   , Option ['p'] ["encfs-pass"]    (ReqArg EncFSPass  "FILE")  "Set encfs password" 
   , Option ['i'] ["rcfile"]        (ReqArg RCFile     "FILE")  "Set rc file path" 
   ]

programOpts :: [String] -> IO ([Flag], [String])
programOpts argv = 
    case getOpt RequireOrder options argv of
        (o,n,[])    -> return (o,n)
        (_,_,errs)  -> ioError (userError (concat errs ++ usageInfo header options))

flagToOption :: HOptions -> Flag -> HOptions  
flagToOption options flag = 
    case flag of 
        Verbose         -> options { verbose = True } 
        EncFSRoot path  -> options { encFSOpts = (encFSOpts options) { root  = path }}
        EncFSMount path -> options { encFSOpts = (encFSOpts options) { mount = path }}
        EncFSPass pass  -> options { encFSOpts = (encFSOpts options) { password  = pass }}
        RCFile path     -> options { optsFile = path }
        otherwise       -> options

setFlagOptions :: HOptions -> [Flag] -> HOptions
setFlagOptions options (x:xs) = setFlagOptions (flagToOption options x) xs
setFlagOptions options []     = options

exitFlags ::  [Flag] -> IO ()
exitFlags [] = return ()
exitFlags flags = do
    case (head flags) of 
        Version -> putStrLn ("hget " ++ hgetVersion) >> exitWith ExitSuccess
        Help    -> putStrLn help >> exitWith ExitSuccess
        otherwise -> return ()

{--
isOptDescr :: OptDescr Flag -> Lisp -> Maybe Flag
isOptDescr (Option _ atom (NoArg arg) _) (List ((Atom at):(Bool s):[])) = 
    if any ((==) at) atom && s == True
        then Just arg
        else Nothing
isOptDescr (Option _ atom (ReqArg arg _) _) (List ((Atom at):(String s):[])) =
    if any ((==) at) atom
        then Just (arg s)
        else Nothing
isOptDescr a b = Nothing

--}
