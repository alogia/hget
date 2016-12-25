module PullContent where

import System.Exit
import System.IO
import System.Process
import System.Directory(getTemporaryDirectory)
import System.FilePath
import Text.Regex.Posix
import Network.URI
import Network.Browser
import Network.HTTP

wgetURI :: (URI, URI) -> FilePath -> String -> IO ExitCode
wgetURI (uri, refererURI) cwd outf = do
    cookie <- getTemporaryDirectory >>= (\d -> openTempFile d "hgetCookie") >>= (\(f, h) -> hClose h >> return f)
    (inp, out, err, pid) <- runInteractiveProcess "/usr/bin/wget" ["-c", "-O" ++ outf, "--keep-session-cookies", "--load-cookies=" ++ cookie, "--save-cookies=" ++ cookie, "--referer=\'" ++ (show $ refererURI) ++ "\'", (uriToString id uri) ""] (Just cwd) Nothing
    outT <- hGetContents out
    outE <- hGetContents err
    exitcode <- waitForProcess pid
    case exitcode of 
        ExitSuccess -> do
                putStrLn $ "Download Complete: " ++ (show $ uri) ++ " >>>> " ++ outf
                return exitcode
        ExitFailure ec -> do 
                print $ "Download failed of: " ++ (show $ uri)
                print "Returned error: "
                print outT
                putStrLn outE
                return exitcode
    
