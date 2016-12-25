module EncFS where

import System.Exit
import System.IO
import System.Process
import System.Directory
import System.FilePath
import System.Console.GetOpt
import Control.Concurrent

data EncFS = 
    EncFS {
      root      :: String, 
      mount     :: String, 
      password  :: String }
      deriving (Eq, Show)

defaultEncFS = 
    EncFS {
        root    = ".hgetRoot",
        mount   = "hgetMount",
        password= "test" }

        
encfsOpen :: EncFS -> IO ()
encfsOpen encOpts = do 
    home <- getHomeDirectory
    (inp, out, err, pid) <- runInteractiveProcess "/usr/bin/encfs" [(home </> root encOpts), (home </> mount encOpts), "--stdinpass"] Nothing Nothing
    hSetBinaryMode inp False
    hPutStr inp (password encOpts) >> hClose inp
    outT <- hGetContents out
    outE <- hGetContents err
    exitcode <- waitForProcess pid
    case exitcode of 
        ExitSuccess -> do
                putStrLn "Mounted"
        ExitFailure ec -> do 
                putStrLn "Error in mounting the encrypted filesystem. Returned error:"
                putStrLn outT
                putStrLn outE
                putStrLn "Exiting hget..."
                exitWith exitcode


encfsClose :: EncFS -> IO ()
encfsClose encOpts = do
    home <- getHomeDirectory
    (inp, out, err, pid) <- runInteractiveProcess "/bin/fusermount" ["-u", (home </> (mount encOpts))] Nothing Nothing 
    outT <- hGetContents out
    outE <- hGetContents err
    exitcode <- waitForProcess pid
    case exitcode of 
        ExitSuccess -> do
                putStrLn "Safely unmounted encrypted filesystem."
        ExitFailure ec -> do 
                putStrLn "Error in unmounting the encrypted filesystem. Returned error:"
                putStrLn outT
                putStrLn outE
                putStrLn "ENCRYPTED FILESYSTEM MAY STILL BE MOUNTED!!!!"
                putStrLn "Exiting hget..."
                exitWith exitcode

