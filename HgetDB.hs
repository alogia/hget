module HgetDB where

import System.IO
import Control.Exception
import Data.List
import Control.Monad
import Data.Maybe
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class(liftIO)
import Database.HDBC 
import Database.HDBC.Sqlite3 
import Network.URI
import Data.Time
import Data.Convertible.Base


linkTableName = "Links"
siteTableName = "Sites"
pageTableName = "Pages"
databaseName = "Hget"

--instance Convertable SqlValue [a] where
    
    
type SqlID = Integer
type HgetDBT a = StateT Connection IO a 


class HTable a where
    getTags  :: a -> HgetDBT [String] 
    getID    :: a -> HgetDBT [SqlID]
    toSql'   :: a -> [SqlValue]
    fromSql' :: [SqlValue] -> a

----------------------------------------------------

data HLink = HLink { 
    linkID      :: Integer,
    linkUrl     :: URI,
    refererUrl  :: URI,
    linkPath    :: FilePath,
    linkDate    :: UTCTime,
    success     :: Bool,
    retry       :: Bool,
    deleted     :: Bool,
    hash        :: String,
    linkTags    :: [String],
    pageRefID   :: SqlID }
        deriving (Eq, Show)

sqlLinkTable = "(linkID INTEGER PRIMARY KEY NOT NULL, linkUrl TEXT, refererUrl TEXT, linkPath TEXT, linkDate TEXT, success INTEGER, retry INTEGER, deleted INTEGER, hash TEXT, linkTags TEXT, pageID INTEGER NOT NULL REFERENCES " ++ linkTableName ++ "(pageID) )" 


----------------------------------------------------



data HPage = HPage {
    pageID      :: SqlID,
    pageUrl     :: URI,
    pagePath    :: String,
    pageDate    :: UTCTime,
    pageTags    :: [String],
    siteRefID   :: SqlID}
        deriving (Eq, Show)

sqlPageTable = "(pageID INTEGER PRIMARY KEY NOT NULL, pageUrl TEXT, pagePath TEXT, pageDate TEXT, pageTags TEXT, siteID INTEGER NOT NULL REFERENCES " ++ pageTableName ++ "(siteID) )" 


----------------------------------------------------


data HSite = HSite {
    siteID      :: SqlID,
    siteUrl     :: URI,
    sitePath    :: FilePath,
    dateAdded   :: UTCTime,
    dateChecked :: UTCTime,
    lispScript  :: String, 
    siteTags    :: [String],
    active      :: Bool }
        deriving (Eq, Show)

sqlSiteTable = "(siteID INTEGER PRIMARY KEY NOT NULL, siteUrl TEXT, sitePath TEXT, dateAdded TEXT, dateChecked TEXT, lispScript BLOB, siteTags TEXT, active INTEGER)"


----------------------------------------------------


whileHDataBase :: FilePath -> HgetDBT b -> IO ()
whileHDataBase path fs = do
    c <- connectH path
    (\i d -> evalStateT d i) c $ do fs
    disconnectH c

connectH :: FilePath -> IO Connection
connectH path = do
    conn <- try $ connectSqlite3 path :: IO (Either SomeException Connection)
    either  (\err -> fail "Could not connect to Hget database") (\conn -> return conn) conn

disconnectH :: Connection -> IO ()
disconnectH con = disconnect con

makeTable :: String -> String -> HgetDBT Integer
makeTable table fields = get >>= (\c -> liftIO $ run c ("CREATE TABLE IF NOT EXISTS " ++ table ++ fields) []) 

makeTables :: HgetDBT () 
makeTables = do 
    makeTable siteTableName sqlSiteTable 
    makeTable pageTableName sqlPageTable
    makeTable linkTableName sqlLinkTable
    commitH

commitH :: HgetDBT ()
commitH = get >>= (liftIO . commit) 


-----------------------------------------------------------


insertLink :: HLink -> HPage -> HgetDBT HLink
insertLink link page = do 
    let l = link { pageRefID = pageID page }
    get >>= (\c -> liftIO $ run c ("INSERT INTO " ++ linkTableName ++ " (linkUrl, refererUrl, linkPath, linkDate, success, retry, deleted, hash, linkTags, pageID) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)") (tail $ toSql' l)) 
    id <- getID l
    return link {linkID = (last id)}

insertPage :: HPage -> HSite -> HgetDBT HPage
insertPage page site = do 
    let p = page { siteRefID = siteID site }
    get >>= (\c -> liftIO $ run c ("INSERT INTO " ++ pageTableName ++ " (pageUrl, pagePath, pageDate, pageTags, siteID) VALUES (?, ?, ?, ?, ?)") (tail $ toSql' p)) 
    id <- getID p
    return page {pageID = (last id)}

insertSite :: HSite -> HgetDBT HSite
insertSite site = do
    get >>= (\c -> liftIO $ run c ("INSERT INTO " ++ siteTableName ++ " (siteUrl, sitePath, dateAdded, dateChecked, lispScript, siteTags, active) VALUES (?, ?, ?, ?, ?, ?, ?)") (tail $ toSql' site)) 
    id <- getID site
    return site {siteID = (last id)}


-----------------------------------------------------------


getLinksForURI :: URI -> HgetDBT [HLink]
getLinksForURI site = do
    c <- get 
    res <- liftIO $ quickQuery' c ("SELECT siteUrl, linkID, linkUrl, refererUrl, linkPath, linkDate, siteID FROM " ++ siteTableName ++ " JOIN " ++  linkTableName ++ " USING ( siteID ) WHERE siteUrl=?") [toSql $ show site]
    return $ map (fromSql' . tail) res


getLinksForPage :: HPage -> HgetDBT [HLink]
getLinksForPage page = do
    c <- get
    res <- liftIO $ quickQuery' c ("SELECT pageID, linkID, linkUrl, refererUrl, linkPath, linkDate, success, retry, deleted, hash, linkTags, pageID FROM " ++ pageTableName ++ " JOIN " ++ linkTableName ++ " USING ( pageID ) WHERE pageID=?") [toSql $ pageID page]
    return $ map (fromSql' . tail) res


getSiteIDURL :: URI -> String -> HgetDBT [SqlID]
getSiteIDURL uri reg = do
    c <- get
    res <- liftIO $ quickQuery' c ("SELECT siteID FROM " ++ siteTableName ++ " WHERE siteUrl=?") [toSql $ show uri]
    return $ map fromSql $ concat res


-----------------------------------------------------------

getIDByURL uri reg = do
    c <- get
    res1 <- liftIO $ quickQuery' c ("SELECT siteID FROM " ++ siteTableName ++ " WHERE siteUrl=?") [toSql $ show uri]
    res2 <- liftIO $ quickQuery' c ("SELECT pageID FROM " ++ pageTableName ++ " WHERE pageUrl=?") [toSql $ show uri]
    res3 <- liftIO $ quickQuery' c ("SELECT linkID FROM " ++ linkTableName ++ " WHERE linkUrl=?") [toSql $ show uri]
    return $ map fromSql $ concat (res1 ++ res2 ++ res3)


-------------------------------------------------------------

instance HTable HLink where

    toSql' link = [
        (toSql $        linkID      link),
        (toSql $ show $ linkUrl     link),
        (toSql $ show $ refererUrl  link),
        (toSql $        linkPath    link), 
        (toSql $        linkDate    link),
        (toSql $        success     link),
        (toSql $        retry       link),
        (toSql $        deleted     link),
        (toSql $        hash        link),
        (toSql $ show $ linkTags    link),
        (toSql $        pageRefID   link)]
            

    fromSql' sql = HLink {
        linkID      = fromSql                           (sql !! 0),
        linkUrl     = (fromJust . parseURI . fromSql)   (sql !! 1),
        refererUrl  = (fromJust . parseURI . fromSql)   (sql !! 2),
        linkPath    = fromSql                           (sql !! 3),
        linkDate    = fromSql                           (sql !! 4),
        success     = fromSql                           (sql !! 5),
        retry       = fromSql                           (sql !! 6),
        deleted     = fromSql                           (sql !! 7),
        hash        = fromSql                           (sql !! 8),
        linkTags    = (read . fromSql)                  (sql !! 9),
        pageRefID   = fromSql                           (sql !! 10)}



    getID link = do
        c <- get
        res <- liftIO $ quickQuery' c ("SELECT linkID FROM " ++ linkTableName ++ " WHERE linkUrl=? AND refererUrl=? AND linkPath=? AND linkDate=? AND success=? AND retry=? AND deleted=? AND hash=? AND linkTags=? AND pageID=? ") (tail $ toSql' link)
        return $ map fromSql $ concat res

    getTags link = do
        c <- get
        res <- liftIO $ quickQuery' c ("SELECT linkTags, pageTags, siteTags FROM " ++ linkTableName ++ " NATURAL JOIN " ++ pageTableName ++ " NATURAL JOIN " ++ siteTableName ++ " WHERE linkID=?") [toSql $ linkID link]
        return $ concat $ map (read . fromSql) $ concat res

---------------------------------------------------------------
instance HTable HPage where 

    toSql' page = [
        (toSql $        pageID      page),
        (toSql $ show $ pageUrl     page),
        (toSql $        pagePath    page),
        (toSql $        pageDate    page),
        (toSql $ show $ pageTags    page),
        (toSql $        siteRefID   page)]

    fromSql' sql = HPage {
        pageID      = fromSql                           (sql !! 0),
        pageUrl     = (fromJust . parseURI .fromSql)    (sql !! 1),
        pagePath    = fromSql                           (sql !! 2),
        pageDate    = fromSql                           (sql !! 3),
        pageTags    = (read . fromSql)                  (sql !! 4),
        siteRefID   = fromSql                           (sql !! 5)}


    getID page = do
        c <- get
        res <- liftIO $ quickQuery' c ("SELECT pageID FROM " ++ pageTableName ++ " WHERE pageUrl=? AND pagePath=? AND pageDate=? AND siteID=?") [toSql $ show $ pageUrl page, toSql $ pagePath page, toSql $ pageDate page, toSql $ siteRefID page]
        return $ map fromSql $ concat res

    getTags page = do
        c <- get
        res <- liftIO $ quickQuery' c ("SELECT pageTags, siteTags FROM " ++ pageTableName ++ " NATURAL JOIN " ++ siteTableName ++ " WHERE pageID=?") [toSql $ pageID page]
        return $ concat $ map (read . fromSql) $ concat res



---------------------------------------------------------------
instance HTable HSite where 

    toSql' site = [
        (toSql $        siteID      site),
        (toSql $ show $ siteUrl     site),
        (toSql $        sitePath    site),
        (toSql $        dateAdded   site),
        (toSql $        dateChecked site),
        (toSql $        lispScript  site),
        (toSql $ show $ siteTags    site),
        (toSql $        active      site)]

    fromSql' sql = HSite {
        siteID      = fromSql                           (sql !! 0),
        siteUrl     = (fromJust . parseURI . fromSql)   (sql !! 1),
        sitePath    = fromSql                           (sql !! 3),
        dateAdded   = fromSql                           (sql !! 4),
        dateChecked = fromSql                           (sql !! 5),
        lispScript  = fromSql                           (sql !! 6),
        siteTags    = (read . fromSql)                  (sql !! 7),
        active      = fromSql                           (sql !! 8)}

    getTags site = do
        c <- get
        res <- liftIO $ quickQuery' c ("SELECT siteTags FROM " ++ siteTableName ++ " WHERE siteID=?") [toSql $ siteID site]
        return $ concat $ map (read . fromSql) $ concat res


    getID site = do
        c <- get
        res <- liftIO $ quickQuery' c ("SELECT siteID FROM " ++ siteTableName ++ " WHERE siteUrl=? AND sitePath=? AND dateAdded=? AND dateChecked=? AND active=?") [toSql $ show $ siteUrl site, toSql $ sitePath site, toSql $ dateAdded site, toSql $ dateChecked site, toSql $ active site]
        return $ map (fromSql . head) res

------------------------------------------------------------------

getAllTags :: HgetDBT [String]
getAllTags = do
    c <- get
    res <- liftIO $ quickQuery' c ("SELECT linkTags, pageTags, siteTags FROM " ++ linkTableName ++ " NATURAL JOIN " ++ pageTableName ++ " NATURAL JOIN " ++ siteTableName) []
    return $ concat $ map (read . fromSql) $ concat res

getSiteTags :: StateT Connection IO [String]
getSiteTags = do
    c <- get
    res <- liftIO $ quickQuery' c ("SELECT siteTags FROM " ++ siteTableName) []
    return $ concat $ map (read . fromSql) $ concat res

getPageTags :: HgetDBT [String]
getPageTags = do
    c <- get
    res <- liftIO $ quickQuery' c ("SELECT pageTags FROM " ++ pageTableName) []
    return $ concat $ map (read . fromSql) $ concat res

getLinksTags :: HgetDBT [String]
getLinksTags = do
    c <- get
    res <- liftIO $ quickQuery' c ("SELECT linkTags FROM " ++ linkTableName) []
    return $ concat $ map (read . fromSql) $ concat res

--------------------

getAllSites :: HgetDBT [HSite]
getAllSites = do
    c <- get 
    sites <- liftIO $ quickQuery' c ("SELECT * FROM " ++ siteTableName) []
    return $ map fromSql' sites

getAllPages :: HgetDBT [HPage]
getAllPages = do
    c <- get 
    pages <- liftIO $ quickQuery' c ("SELECT * FROM " ++ pageTableName) []
    return $ map fromSql' pages

getAllLinks :: HgetDBT [HLink]
getAllLinks = do
    c <- get 
    links <- liftIO $ quickQuery' c ("SELECT * FROM " ++ linkTableName) []
    return $ map fromSql' links
