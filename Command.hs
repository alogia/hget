module Command where

import Control.Monad hiding (unless)
import Data.List.Utils
import Control.Monad.Trans.Error
import Control.Monad.State.Lazy hiding (unless)
import IO(FilePath)


data CTable  = Site   
             | Page   
             | Link   
             | Feed
             | All    
             deriving (Show,Eq)

data CTag =    ID       Integer
             | URL      String
             | Tags    [String]
             | Hash     String
             | BasePath FilePath
             | Type     CType
             | Script   FilePath
             | PageMask String
             | To       CTo
             | File     FilePath
             deriving (Show,Eq)

data CTo =     Tar    FilePath
             | BZip   FilePath
             | GZip   FilePath
             | Zip    FilePath
             | Dir    FilePath
             deriving (Show,Eq)

data CType =   Video 
             | Audio
             | Image
             | Regex String
             deriving (Show,Eq)


data CComm =   Query  CTable [CTag]
             | Edit   CTable [CTag]
             | Add    CTable [CTag]
             | Remove CTable [CTag]
             | Purge  CTable [CTag]
             | Get    CTable [CTag]
             | Test   [CTag]

             | Update
             | Download

             | Pause  CTable [CTag]
             | Start  CTable [CTag]
             deriving (Show,Eq)

type CommandT a = ErrorT String (State [String]) a

getCommand :: [String] -> Either String CComm
getCommand args = evalState (runErrorT getModify) args
                
getModify :: CommandT CComm
getModify = do
    mod <- getNext
    case mod of 
        "query" -> do
            t <- try getTable All
            f <- getFields [] ["to", "script", "pagemask", "basepath"] ["url", "id", "tags", "hash", "file"]
            return $ Query t f
        "edit" -> do 
            t <- getTable
            f <- case t of 
                Site -> getFields ["id"] ["to", "basepath", "url", "hash", "file"] ["tags", "script", "pagemask", "type"]
                Page -> getFields ["id"] ["to", "basepath", "url", "hash", "file"] ["tags", "type", "script"]
                Link -> getFields ["id"] ["to", "script",  "basepath", "url", "hash", "type", "file"] ["tags"]
                Feed -> getFields ["id"] ["to", "script",  "basepath", "url", "hash", "type", "file"] ["tags"]
                All  -> fail  "Error: 'all' is not a valid table type when editing"
            return $ Edit t f 
        "add"   -> do
            t <- getTable 
            f <- case t of 
                Site -> getFields ["url"] ["to", "id", "hash", "file"]                               ["script", "pagemask"] 
                Page -> getFields ["url"] ["to", "id", "hash", "pagemask", "file"]                   ["script", "type"] 
                Link -> getFields ["url"] ["to", "id", "hash", "type", "script", "pagemask", "file"] [] 
                Feed -> getFields ["url"] ["to", "id", "hash", "type", "script", "pagemask", "file"] [] 
                All  -> fail  "Error: 'all' is not a valid table type when adding"
            return $ Add t f 
        "remove"-> do
            t <- try getTable All
            f <- getFields [] ["to", "script", "pagemask", "basepath"] ["url", "id", "tags", "hash", "file"]
            return $ Remove t f
        "purge" -> do
            t <- try getTable All
            f <- getFields [] ["to", "script", "pagemask", "basepath"] ["url", "id", "tags", "hash", "file"]
            return $ Remove t f
        "pause" -> do
            t <- try getTable All
            f <- getFields [] ["to", "script", "pagemask", "basepath"] ["url", "id", "tags", "hash"]
            return $ Pause t f
        "start" -> do
            t <- try getTable All
            f <- getFields [] ["to", "script", "pagemask", "basepath"] ["url", "id", "tags", "hash"]
            return $ Start t f
        "get" -> do 
            t <- getTable 
            f <- case t of 
                Site -> getFields ["url", "to"] ["id", "tags", "hash", "file"]                               ["script", "pagemask"] 
                Page -> getFields ["url", "to"] ["id", "tags", "hash", "pagemask", "file"]                   ["script", "type"] 
                Link -> getFields ["url", "to"] ["id", "tags", "hash", "type", "script", "pagemask", "file"] [] 
                Feed -> getFields ["url", "to"] ["id", "tags", "hash", "type", "script", "pagemask", "file"] [] 
                All  -> fail  "Error: 'all' is not a valid table type when adding"
            return $ Get t f
        "update" -> return $ Update
        "download" -> return $ Download
        otherwise -> fail "Invalid Command"

getTo :: String -> Either String CTo
getTo to = let st = split "." to  
               len = length st 
               err = "Cannot determine filetype in 'to=' \nSupported filetypes are:\n   foo/ -- Directory\n   foo.tar -- Tar Archive\n   foo.tar.gz -- Gzip Archive\n   foo.tar.bz2 -- Bzip Archive\n   foo.zip -- Zip Archive"
           in if (len >= 3) 
                then if st !! (len - 2) == "tar"
                     then case st !! (len - 1) of
                            "gz"  -> Right $ GZip to
                            "bz2" -> Right $ BZip to
                            otherwize -> Left err
                     else Left err
                else if (len >= 2)  
                        then case st !! (len -1) of
                                "tar"     -> Right $ Tar to
                                "zip"     -> Right $ Zip to
                                otherwise ->  Left err
                        else if (len == 1)
                                then Right $ Dir to
                                else Left err
                          

getTable :: CommandT CTable
getTable = do
    table <- getNext
    case table of
        "site" -> return Site 
        "page" -> return Page
        "link" -> return Link 
        "feed" -> return Feed
        "all"  -> return All
        otherwise -> fail "Error: This command requires a link type:\n   site - A url which links to other pages who link to content\n   page - A url which links to content directly\n   link - A direct url to content for download\n feed - An xml RSS/Atom feed"

getFields :: [String] -> [String] -> [String] -> CommandT [CTag]
getFields required banned oneOf = do
    fs <- get
    let fields = map parse fs
    unless (all (\f -> f `elem` (map fst fields)) required) ("Error: You have not passed the command a required field:\n" ++ (foldl (\x y -> y ++ "=?\n" ++ x) "" required))
    if length oneOf /= 0 
        then unless (any (\(f,_) -> f `elem` oneOf) fields) ("Error: You have not passed the command enough fields. Use one of:\n" ++ (foldl (\x y -> y ++ "=?\n" ++ x) "" oneOf))
        else return ()
    failWhen (any (\(f,_) -> f `elem` banned) fields) ("Error: You have passed the command one of these invalid fields:\n" ++ (foldl (\x y -> y ++ "=?\n" ++ x) "" banned))
    fields <- mapM getField fields
    if (any (\f -> case f of Type _ -> True; otherwise -> False) fields) && (any (\f -> case f of Script _ -> True; otherwise -> False) fields)
        then fail "Error: Cannot use both 'script=' and 'type=' fields"
        else return fields
    where 
        parse field = 
            let fs = (split "=" field)
                val = last fs
                f = head fs
            in (f, val)
        getField (f, val) = do
            case f of
                "id"        -> return $ ID (read val) 
                "url"       -> return $ URL val
                "tags"      -> return $ Tags $ split "," val
                "hash"      -> return $ Hash val
                "script"    -> return $ Script val
                "type"      -> return $ Type (getType val)
                "pagemask"  -> return $ PageMask val 
                "file"      -> return $ File val
                "to"        -> case getTo val of
                                Left err -> fail err
                                Right to -> return $ To to
                otherwise   -> fail "Invalid field(s) or name"

getType :: String -> CType
getType str = case str of 
                "video"     -> Video
                "audio"     -> Audio
                "images"    -> Image
                otherwise   -> Regex str

getNext ::  CommandT String
getNext = do
    table <- get
    put (tail table)
    return $ (head table)

unless :: Bool -> String -> CommandT ()
unless f m = do
    case f of 
        True -> return ()
        False -> fail m

failWhen :: Bool -> String -> CommandT ()
failWhen f m = do
    case f of 
        False -> return ()
        True -> fail m

try :: CommandT b -> b -> CommandT b 
try f a = do
    now <- get
    catchError f (\_-> put now >> return a) 

