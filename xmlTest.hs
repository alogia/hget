import XmlParse

import Text.XML.Light
import Network.URI(parseURI)
import System(getArgs)

main = do
    args <- getArgs
    f <- readFile $ head args
    mapM_ print $ concat $ map getLinks $ onlyElems $ parseXML f 
