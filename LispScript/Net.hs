{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
module LispScript.Net where

import Data.Monoid
import Data.Either
import Network.URI
import Network.Browser
import Network.HTTP
import System.IO
import Control.Monad.IO.Class(liftIO)
import Text.XML.HXT.Core
--import Text.XML.HXT.Curl
import Text.HandsomeSoup
import Text.Parsec

import General
import LispScript
import PullContent

runL file = readFile file >>= (\f -> runX (htmlToLisp f >>> deep (hasName "html") >>> nodeToLisp)) >>= return.List

htmlToLisp :: String -> IOStateArrow () XmlTree XmlTree
htmlToLisp html = readString [ withParseHTML    yes, 
                               withWarnings     no, 
                               withRemoveWS     yes ] html 
                  
nodeToLisp :: IOStateArrow () XmlTree Lisp
nodeToLisp = isElem >>> proc a -> do 
                    nam     <- getElemName -< a
                    rest    <- listA (nodeToLisp <<< getChildren) -< a
                    avals   <- listA $ (getAttrName >>> arr localPart) 
                               &&& (getQAttrValue $< (getAttrName <<< getAttrl)) -< a
                    returnA -< List $ [Atom (localPart nam), AList $ map (\(x, y) -> (Atom x, Atom y)) avals] ++ rest
                        

httpRequestFunc :: Function 
httpRequestFunc = Internal "http-request" (ReqArgs [(Atom "uri")]) $ do 
    uri <- isString 0 httpRequestFunc
    case parseURI uri of 
        Just u -> liftIO $ getHTTP u >>= return.toLisp
        Nothing -> fail $ "Error: String passed to function " ++ (atom httpRequestFunc) ++ " is not a proper URI."
