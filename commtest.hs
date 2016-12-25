import Command 
import System(getArgs)
import IO

main = do
    x <- getArgs
    case getCommand x of 
        Left err  -> putStrLn err
        Right com -> print com
