module XmlParse where

import Maybe
import Network.URI
import Text.XML.Light

getLinks ::  Element -> [URI]
getLinks elem  = catMaybes $ map parseURI $ catMaybes $ map (findAttrBy isHref) $ filterElementsName isA elem
    where isA qn =  "a" == qName qn
          isHref qn =  "href" == qName qn

