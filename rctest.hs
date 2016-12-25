import Options
import LispScript.Hget

import System.IO
import Data.Maybe

main = do
    (l, s) <- runHgetScript $ optsFile defaultHOptions 
    print l

