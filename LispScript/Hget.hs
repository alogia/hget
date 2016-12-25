module LispScript.Hget where

import Control.Monad

import LispScript
import Options 
import EncFS
import PullContent
import Types

 
 
defaultOptAtoms :: [BoundAtom]
defaultOptAtoms = [   (Atom "verbose"   ,    toLisp (verbose defaultHOptions)) 
                  ,   (Atom "rcfile"    ,    toLisp (optsFile defaultHOptions)) 
                  ,   (Atom "dataBase"  ,    toLisp (dataBase defaultHOptions)) 
                  ,   (Atom "encfsroot" ,    toLisp (root defaultEncFS))
                  ,   (Atom "encfsmount",    toLisp (mount defaultEncFS))
                  ,   (Atom "encfspass" ,    toLisp (password defaultEncFS)) ]

typeAtoms = [ (Atom "sound-ext" , toLisp soundExt)
            , (Atom "image-ext" , toLisp imageExt)
            , (Atom "video-ext" , toLisp videoExt) ]

runHgetScript :: FilePath -> IO (Lisp, LispState)
runHgetScript input = do 
    res <- runLispScript input defaultLispState 
    case res of 
        Just (l, s) -> return (remComments l, s)
        Nothing     -> fail "Error: Failed to parse init file."


{-- This monad type should allow for a string to be loaded as 
 - a state when run, and then any number of "replace" actions
 - can be run on it, which will replace an escaped sequence 
 - (e.g. %D with a date). When the monad completes, it will 
 - return the string with all replacements which where ran 
 - on it. 
 --}

type Replacement = (Char, String)
newtype Replacer a = Replacer {runRep :: (a, String)} 

replacements = [('S', "blah"),
                ('R', "foo"),
                ('B', "Bar") ]


--replace a = do 
--    Replacer ("", , 

instance Monad Replacer where
        f >>= g = let (a, s) = runRep f 
                      n = g a
                      (a2, s2) = runRep n 
                  in Replacer (a2, s2)
             
        return a = Replacer (a, [])


-----------------------------------------------------

data HScript = HScript {
    hashNames   :: Bool,
    nameMask    :: String}

