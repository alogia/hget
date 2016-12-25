import Control.Monad.State.Lazy
import Database.HDBC

import Data.Maybe(fromJust)
import System.Directory(getHomeDirectory)
import Data.Time.Clock(getCurrentTime)
import Network.URI(parseURI)
import Control.Monad.IO.Class(liftIO)

import HgetDB

hgetDB = "dbtest.db"

siteDo = do
    home <- getHomeDirectory
    now <- getCurrentTime
    return HSite {
        siteID      = 0,
        siteUrl     = fromJust $ parseURI "http://www.wee.com",
        sitePath    = home,
        dateAdded   = now,
        dateChecked = now,
        lispScript  = "aoeu",
        siteTags    = ["foo", "bar"],
        active      = True }

pageDo :: IO HPage
pageDo = do 
    now <- getCurrentTime
    return HPage {
        pageID      = 0,
        pageUrl     = fromJust $ parseURI "http://www.somepage.com/files/wee",
        pagePath    = "/somepage/wee/",
        pageDate    = now,
        pageTags    = ["wee"],
        siteRefID   = 0 }

linkDo = do 
    home <- getHomeDirectory
    now <- getCurrentTime
    return HLink {
        linkID      = 0,
        linkUrl     = fromJust $ parseURI "http://www.google.com",
        refererUrl  = fromJust $ parseURI "http://www.gmail.com",
        linkPath    =  home,
        linkDate    =  now,
        success     = True,
        retry       = False,
        deleted     = False,
        hash        = "snathoeunsht",
        linkTags    = ["Tyler", "Moo"],
        pageRefID   =  0
        }


main = do
    whileHDataBase hgetDB $ do

        putStr "Creating tables"
        makeTables

        putStr "Inserting site"
        site <- liftIO $ siteDo
        s <- insertSite site
        pr s
        pr $ "Site has id: " ++ (show $ siteID s)

        putStr "Inserting Page"
        page <- liftIO $ pageDo
        p <- insertPage page s
        pr p
        pid <- getID page 
        pr $ "Page has id: " ++ (show $ pageID p)


        putStr "Inserting Link"
        link <- liftIO $ linkDo
        l <- insertLink link p

        putStr "\nTags for link is: "
        tags <- getTags l
        pr tags

        commitH

        putStr "Getting Page Links:"  
        ln <- getLinksForPage p
        pr ln 

        where   
            putStr s = (liftIO . putStrLn) s
            pr s = (liftIO . print) s 


