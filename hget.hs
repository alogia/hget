module Main where

import System.Exit
import System.Environment
import System.IO
import System.Process
import System.Directory
import System.FilePath
import Network.URI(parseURI)
import Data.Maybe(fromJust)
import Text.Parsec
import Data.List

import LispScript
import LispScript.Net
import EncFS
import PullContent
import HgetDB
import Options
import Daemon 


main = do 

    (flags, nonOpts) <- programOpts =<< getArgs
    exitFlags flags
    home <- getHomeDirectory  

    let opts = setFlagOptions defaultHOptions flags
    print opts

    createDaemon "9000" defaultLispState doLisp 
    where 
        doLisp state (addr,fh) input = do 
            res <- runParserT returnLisp state (show addr) input 
            case res of 
               Left e          -> hPutStrLn fh ("Error: " ++ (show e)) >> return state
               Right (val, st) -> hPutStrLn fh (show val)  >> return st


--    print actions
--    print nonOpts
--    putStrLn "Mounting encypted volume"
--    encfsOpen defaultEncFS

--    putStrLn "Getting Index..."
--    index <- getIndex (head nonOpts) [] [] 
--    print $ parseIndex index
--    pullVid ((fromJust . parseURI) (head nonOpts)) $ (home </> (mount defaultEncFS))

    
--    putStrLn "Unmounting encrypted volume"
--    encfsClose defaultEncFS
--    exitWith ExitSuccess

