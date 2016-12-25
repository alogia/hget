module Daemon where

import System.IO
import System.Exit
import System.Posix.Daemonize
import Network.Socket
import Network.BSD
import Control.Monad (forever)
import qualified Control.Monad.Exception as E
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Reader

import General

type ConnHandler a = a -> (SockAddr, Handle) -> String -> IO a 

welcome = "Welcome to Hget!"
help = "Hget Commands: \n\
    \\t :help           -- Shows this message \n\
    \\t :quit           -- Quits the connect to hget \n\
    \\t :version        -- Shows hget's version"
    

createDaemon ::  ServiceName -> a -> ConnHandler a -> IO ()
createDaemon port initState func = (daemonize.withSocketsDo) $ do
    addrinfo <- getAddrInfo (Just $ defaultHints{addrFlags=[AI_PASSIVE]}) 
                           Nothing
                           (Just port)
    let saddr = head addrinfo
    sock <- socket (addrFamily saddr) Stream defaultProtocol
    bindSocket sock (addrAddress saddr)
    listen sock 1 
    mState <- newMVar initState
    mainId <- myThreadId
    forever $ procRequests mainId mState sock
    where 
        procRequests contTh mState sock = do
            (con,clsock) <- accept sock
            --TODO: Log connects and stuff
            forkIO $ runReaderT (procPackets con clsock) (contTh, mState) 
        procPackets conn client = do
            E.bracket (liftIO $ socketToHandle conn ReadWriteMode) (liftIO . hClose) $ \hand -> do
                liftIO $ hPutStrLn hand $ welcome
                liftIO $ hSetBuffering hand NoBuffering
                procCommand (client, hand)
        procCommand client@(addr,hand) = do 
            liftIO $ hPutStr hand $ "Hget: " ++ show addr ++ " -> "
            command <- liftIO $ hGetLine hand 
            fn <- getAction command
            fn client 
            procCommand client
        handler client@(addr,hand) comm = do 
            (contTh, mState) <- ask
            state <- liftIO $ takeMVar mState
            res <- liftIO $ func state client comm
            liftIO $ putMVar mState res 
        getAction str = do 
            return $ if head str == ':'
                        then \cl -> internCommand cl $ (init.tail) str
                        else \cl -> handler cl str
        internCommand (addr,hand) str = do
            (contTh, mState) <- ask
            liftIO $ case str of 
                "quit"      -> hPutStrLn hand "Quiting...." >> exitSuccess
                "kill"      -> hPutStrLn hand "Server is now closing..." >> exitSuccess
                "help"      -> hPutStrLn hand help
                "version"   -> hPutStrLn hand $ "Hget " ++ hgetVersion
                otherwise   -> hPutStrLn hand $ "Unknown command: " ++ str

