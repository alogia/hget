{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module LispScript where

import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Pos
import Text.Parsec
import Text.Regex.Posix
import Text.Regex.Base
import Control.Monad
import Control.Monad.Identity (Identity)
import GHC.Float (double2Float)
import Data.Maybe
import Data.List
import Data.Char(toUpper, toLower)
import Data.Monoid
import System.Random (randomRIO)
import System.IO
import System.IO.Error 
import System.Directory(doesFileExist)
import Text.Parsec.Prim(modifyState)
import System.FilePath.Posix
import Data.Unique
import Control.Monad.IO.Class(liftIO)

{-- NEEDED FUNCTIONS
 - cons
 - labels
 - defparameter
 - setf
 - apply
 - remove-if
 - list
 - funcall
 - sort
 - reverse
 -
 - NEEDED CONTROL STRUCTURES 
 - do
 - #' function arguments 
 --}
 
type LParser r = ParsecT [Char] LispState IO r

version = (0,1)

--                Atom  Value
type BoundAtom = (Lisp, Lisp)

data Args = ReqArgs  [Lisp]
          | RestArgs [Lisp] Lisp  
          | OptArgs  [Lisp] [Lisp]

newtype FileHandle = FileHandle (Unique, FilePath)

data Lisp =    Atom         String 
            |  List         [Lisp]
            |  AList        [(Lisp, Lisp)]
            |  Integer      Integer
            |  Float        Float
            |  String       String
            |  File         FileHandle
            |  Comment      String
            |  T
            |  Nil
            deriving Ord

class Lispable a where
    toLisp   :: a -> Lisp
    fromLisp :: Lisp -> a

instance Lispable [Char] where
    toLisp st = String st
    fromLisp (String st) = st
    fromLisp (Atom at) = at

instance Lispable a => Lispable [a] where
    toLisp lst = List $ map toLisp lst
    fromLisp (List l) = map fromLisp l

instance (Lispable a, Lispable b) => Lispable [(a, b)] where
    toLisp alst = AList $ map (\(x, y) -> (toLisp x, toLisp y)) alst
    fromLisp (AList alst) = map (\(x, y) -> (fromLisp x, fromLisp y)) alst

instance Lispable Integer where
    toLisp i = Integer i
    fromLisp (Integer i) = i

instance Lispable Float where
    toLisp f = Float f
    fromLisp (Float f) = f

instance Lispable Bool where
    toLisp True  = T
    toLisp False = Nil
    fromLisp T = True
    fromLisp Nil = False

data LispState =  LispState {
       workingDir  :: FilePath,
       logFile     :: FilePath, 
       functions   :: [Function],
       atoms       :: [BoundAtom], 
       fileHandles :: [(Lisp, Handle)], 
       shouldParse :: (Bool, Lisp) }

internalFuncs = [equalFunc, lessThanFunc, lessThanOrEqualFunc, moreThanFunc, moreThanOrEqualFunc, regexFunc, 
                 cdrFunc, carFunc, probefileFunc, concatenateFunc, mapcarFunc, loadFunc, 
                 appendFunc, pushFunc, lengthFunc, openFunc, closeFunc, readcharFunc, readlineFunc, 
                printFunc, nullFunc, notFunc, plusFunc, minusFunc, multFunc, divideFunc, andFunc, orFunc,
                sinFunc, cosFunc, tanFunc, expFunc, exptFunc, sqrtFunc, incfFunc, randomFunc]

defaultLispState  = LispState {atoms=[], workingDir=".", logFile="", functions=internalFuncs, fileHandles=[], shouldParse=(True, Nil)} 

instance Monoid Lisp where
    mappend (List a)    (List [])   = List a
    mappend (List [])   (List b)    = List b 
    mappend (List a)    (List b)    = List $ a ++ b
    mappend (List [])   T           = Nil
    mappend T           (List [])   = Nil
    mappend (AList a)   (AList b)   = AList $ a ++ b
    mappend (Integer a) (Integer b) = Integer $ a + b
    mappend (Integer a) (Float b)   = Float $ (fromIntegral a) + b
    mappend (Float a)   (Integer b) = Float $ a + (fromIntegral b)
    mappend (Float a)   (Float b)   = Float $ a + b 
    mappend (String a)  (String b)  = String $ a ++ b
    mappend T           Nil         = Nil
    mappend Nil         T           = Nil
    mappend (Comment a) (Comment b) = Comment $ a ++ b
    mappend (List a)    b           = List $ a ++ [b]
    mappend Nil         Nil         = Nil
    mappend a           b           = List [a, b]
    mempty = List []
--    mconcat list = concatLists list

instance Eq Lisp where
     List []      == Nil        = True
     Nil          == List []    = True
     List []      == AList []   = True
     AList []     == List []    = True
     AList []     == Nil        = True
     Nil          == AList []   = True
     Atom a       == Atom b     = map toLower a == map toLower b
     List a       == List b     = a == b 
     AList a      == AList b    = a == b
     Integer a    == Integer b  = a == b
     Float a      == Float b    = a == b
     String a     == String b   = a == b
     File a       == File b     = a == b
     Comment a    == Comment b  = a == b
     T            == T          = True
     Nil          == Nil        = True 
     a            == b          = False 
     a            /= b          = True

instance Show Lisp where
    show  (Atom a)      = map toUpper a
    show  (List [])     = "NIL"
    show  (List l)      = "(" ++ (init (concat (map (\x -> show x ++ " ") l))) ++ ")"
    show  (AList [])    = "NIL"
    show  (AList l)     = "(" ++ (init (concat (map (\(x, y) -> "(" ++ (show x) ++ " " ++ (show y) ++ ") ") l))) ++ ")" 
    show  (Integer a)   = show a     
    show  (Float a)     = show a
    show  (String a)    = "\"" ++ a ++ "\""
    show  (File f)      = show f
    show  (Comment a)   = a
    show  T             = "T"
    show  Nil           = "NIL"

instance Eq FileHandle where
    FileHandle (u1, _) == FileHandle (u2, _) = u1 == u2 

instance Show FileHandle where
    show (FileHandle (i, f)) = show "HandleID: " ++ show (hashUnique i) ++ " --- Path: " ++ f 

instance Ord FileHandle where
    FileHandle (i1, f1) <= FileHandle (i2, f2) = i1 <= i2 

--                       atom   args func
data Function = External String Args [Char] 
              | Internal String Args (LParser Lisp) 

funcPos :: Function -> [Char]
funcPos (External _ _ pos) = pos

algo :: Function -> LParser Lisp
algo (Internal _ _ al) = al

atom :: Function -> String
atom (External at _ _) = at
atom (Internal at _ _) = at

args :: Function -> Args
args (External _ ar _) = ar
args (Internal _ ar _) = ar

parseWhile :: Function -> [BoundAtom] -> LParser Lisp
parseWhile func ars = 
    case func of 
        External _ _ _ -> 
            withAtoms ars $ do 
                retPos <- getInput 
                setInput $ funcPos func
                res <- parseSExpr 
                setInput retPos
                getState >>= setState . (removeFunc ((External "lambda" (ReqArgs []) []))) 
                return res
        Internal _ _ _ -> withAtoms ars (algo func) 

addFunc :: Function -> LispState -> LispState
addFunc func state = state{functions=func:(functions state)}

removeFunc :: Function -> LispState -> LispState
removeFunc func state = 
    state{functions=deleteBy delBy func (functions state)}
        where
            delBy x y = let f1 = atom x 
                            f2 = atom y
                        in f1 == f2 

removeAtom :: BoundAtom -> LispState -> LispState
removeAtom a state = state {atoms=deleteBy (\x y-> fst x == fst y) a (atoms state)}

undefineAtoms :: [BoundAtom] -> LispState -> LispState
undefineAtoms [] state = state 
undefineAtoms (a:as) state = undefineAtoms as (state {atoms=deleteBy (\x y -> fst x == fst y) a (atoms state)})

zipWithNil :: [Lisp] -> [BoundAtom]
zipWithNil ats = zip ats (replicate (length ats) Nil)

undefineArgs :: Function -> LispState -> LispState 
undefineArgs func state = 
    case (args func) of
        (ReqArgs ats)               -> undefineAtoms (zipWithNil ats) state
        (RestArgs ats (List rest))  -> undefineAtoms (zipWithNil $ ats ++ rest) state
        (OptArgs ats opts)          -> undefineAtoms (zipWithNil $ ats ++ opts) state

dropAtoms :: [BoundAtom] -> LispState -> LispState
dropAtoms [] state = state
dropAtoms (a:as) state = dropAtoms as $ state{ atoms = dropWhile (\x -> fst x == fst a) (atoms state)}

withAtoms :: [BoundAtom] -> LParser a -> LParser a
withAtoms ats exp = do 
        getState >>= setState . (defineAtoms ats)
        res <- exp 
        getState >>= setState . (undefineAtoms ats)
        return res

getFuncByAtom :: Lisp -> LispState -> Maybe Function
getFuncByAtom (Atom at) state = nextFunc at (functions state) 
    where nextFunc at []     = Nothing 
          nextFunc at (f:fs) = if at == atom f 
                                then Just f 
                                else nextFunc at fs
getFuncByAtom at _ = Nothing

parseArgs :: Function -> LParser [BoundAtom]
parseArgs func = do
    let as = args func
    res <- try $ parseExpr `sepBy` spaces 
    try spaces 
    let lenRes = length res 
    case as of 
        ReqArgs req       -> if lenRes == (length req)
                                then return $ zip req res
                                else fail $ "Cannot call function \'" ++ (atom func) ++ "\' : Wrong number of arguments."
        RestArgs req rest -> if lenRes == (length req)
                                then return $ zip req res
                                else if lenRes > (length req)
                                    then return $ zip req (take ((length req)) res) ++ [(rest, List (drop ((length req)) res))]
                                    else fail $ "Cannot call function \'" ++ (atom func) ++ "\' : Wrong number of arguments."
        OptArgs req opts -> if lenRes == (length req)
                                then return $ zip req res
                                else if lenRes > (length req) && lenRes <= ((length req) + (length opts))  
                                    then return $ zip (take lenRes (req ++ opts)) res
                                    else fail $ "Cannot call function \'" ++ (atom func) ++ "\' : Wrong number of arguments."

parseFuncCall :: Function -> LParser Lisp
parseFuncCall func = (istring.atom) func >> spaces >> parseArgs func >>= parseWhile func

defineAtoms :: [BoundAtom] -> LispState -> LispState
defineAtoms ats state = state{ atoms=(ats++(atoms state)) }

getAtom :: Lisp -> LispState -> Maybe Lisp
getAtom atom state = lookup atom (atoms state)

ichar c = char (toLower c) <|> char (toUpper c)
istring s = mapM ichar s

sexp f = between (char '(' >> (try spaces)) ((try spaces) >> char ')') f

symbol :: LParser Char
symbol = oneOf "!#$%&\\*+-/:<=>?@^_~"

trueAtom :: LParser Lisp
trueAtom  = istring "t" >> return T   

nilAtom :: LParser Lisp
nilAtom = istring "nil" >> return Nil

parseAtom :: LParser Lisp
parseAtom = try trueAtom 
        <|> try nilAtom 
        <|> do  first <- letter <|> symbol
                rest  <- many (letter <|> digit <|> symbol)
                return $ Atom $ ([first] ++ rest)

skipAny1 :: LParser [Char] 
skipAny1 = many1 (noneOf "()") 

skipSexp :: LParser [Char]
skipSexp = char '(' >> unNest 1 ['(']
    where 
        unNest 0 acc  = return acc
        unNest num acc = do 
            up <- optionMaybe $ char ')'
            case up of
                Just c  -> unNest (num - 1) (acc ++ [c])
                Nothing -> do
                    down <- optionMaybe $ char '('
                    case down of
                        Just r  -> unNest (num + 1) (acc ++ [r])
                        Nothing -> skipAny1 >>= \x -> unNest num (acc ++ x)

skipExpr :: LParser [Char]
skipExpr = skipSexp <|> skipAny1

parseDefun :: LParser Lisp
parseDefun = sexp $ do
    istring "defun" >> spaces
    Atom at <- parseAtom
    spaces
    state <- getState
    if any (\f -> atom f == at) $ functions state
        then fail $ "The function \'" ++ at ++ "\' is already defined."  -- <<---Make this error fatal
        else do ars <- sexp $ parseNext []
                spaces
                sexp <- skipSexp
                setState $ addFunc (External at ars sexp) state
                return $ Atom at
    where 
        parseNext res =  try (parseRest >>= \r -> return (RestArgs res r))
                     <|> try (parseOpt  >>= \o -> return (OptArgs res o))
                     <|> try (parseReq  >>= \r -> parseNext (res ++ [r]))
                     <|> return (ReqArgs res)
        parseReq = do
            at <- parseAtom 
            try spaces 
            return at
        parseRest = do
            istring "&rest" >> spaces 
            rest <- parseAtom 
            return rest
        parseOpt = do
            istring "&optional" >> spaces  
            opts <- parseAtom `sepBy` spaces
            return opts

firstArg :: Function -> Lisp
firstArg func = case (args func) of 
    ReqArgs  (a:as)   -> a
    RestArgs (a:as) _ -> a
    OptArgs  (a:as) _ -> a

secondArg :: Function -> Lisp
secondArg func = case (args func) of 
    ReqArgs  (a:b:as)   -> b
    RestArgs (a:b:as) _ -> b
    OptArgs  (a:b:as) _ -> b

lastArg :: Function -> Lisp
lastArg func = case (args func) of 
    ReqArgs  as    -> last as
    RestArgs as r  -> r
    OptArgs  as os -> last os

reqArgs :: Function -> [Lisp]
reqArgs func = case (args func) of
    ReqArgs  as    -> as
    RestArgs as r  -> as
    OptArgs  as os -> as

restArg :: Function -> Lisp
restArg func = case (args func) of
    ReqArgs  as    -> Nil
    RestArgs as r  -> r
    OptArgs  as os -> Nil

optArgs :: Function -> Lisp
optArgs func = case (args func) of
    ReqArgs  as    -> Nil
    RestArgs as r  -> Nil
    OptArgs  as os -> List os

nArg :: Int -> Function -> Lisp 
nArg n func = case (args func) of 
    ReqArgs  as    ->  as !! n
    RestArgs as r  -> (as ++ [r]) !! n
    OptArgs  as os -> (as ++ os) !! n

nArgVal :: Int -> Function -> LispState -> Lisp
nArgVal n func state = fromJust $ getAtom (nArg n func) state

equalFunc :: Function
equalFunc = Internal "eq" (ReqArgs [(Atom "val1"), (Atom "val2")]) $ do
    v1 <- isObject 0 equalFunc
    v2 <- isObject 1 equalFunc
    return $ toLisp $ v1 == v2

lessThanFunc :: Function
lessThanFunc = Internal "<" (RestArgs [(Atom "val1")] (Atom "vals")) $ do 
    n1 <- isNumber 0 lessThanFunc 
    rest <- isNumList 1 lessThanFunc 
    return $ toLisp $ and $ map (< n1) rest

lessThanOrEqualFunc :: Function
lessThanOrEqualFunc = Internal "<=" (RestArgs [(Atom "val1")] (Atom "vals")) $ do
    n1 <- isNumber 0 lessThanOrEqualFunc 
    rest <- isNumList 1 lessThanOrEqualFunc 
    return $ toLisp $ and $ map (<= n1) rest

moreThanFunc :: Function
moreThanFunc = Internal ">" (RestArgs [(Atom "val1")] (Atom "vals")) $ do
    n1 <- isNumber 0 moreThanFunc 
    rest <- isNumList 1 moreThanFunc 
    return $ toLisp $ and $ map (> n1) rest
    
moreThanOrEqualFunc :: Function
moreThanOrEqualFunc = Internal ">=" (RestArgs [(Atom "val1")] (Atom "vals")) $ do
    n1 <- isNumber 0 moreThanOrEqualFunc 
    rest <- isNumList 1 moreThanOrEqualFunc 
    return $ toLisp $ and $ map (>= n1) rest

andFunc :: Function
andFunc = Internal "and" (RestArgs [] (Atom "forms")) $ do
    list <- isList 0 andFunc
    return $ case foldM appAnd T list of
        Just a  -> a
        Nothing -> Nil
    where appAnd acc val = case val of
                                List []    -> Nothing
                                AList []   -> Nothing
                                Nil        -> Nothing
                                a          -> Just a

orFunc :: Function
orFunc = Internal "or" (RestArgs [] (Atom "forms")) $ do
    list <- isList 0 orFunc
    return $ nextOr list
    where nextOr (l:ls) = case l of 
                            Nil        -> nextOr ls
                            List []    -> nextOr ls
                            AList []   -> nextOr ls
                            r          -> r

cosFunc :: Function 
cosFunc = Internal "cos" (ReqArgs [(Atom "number")]) $ isFloat 0 cosFunc >>= return . Float . cos 
            
sinFunc :: Function 
sinFunc = Internal "sin" (ReqArgs [(Atom "number")]) $ isFloat 0 sinFunc >>= return . Float . sin
            
tanFunc :: Function 
tanFunc = Internal "tan" (ReqArgs [(Atom "number")]) $ isFloat 0 tanFunc >>= return . Float . tan 

expFunc :: Function 
expFunc = Internal "exp" (ReqArgs [(Atom "number")]) $ toFloat 0 expFunc >>= return . Float . exp 

exptFunc :: Function 
exptFunc = Internal "expt" (ReqArgs [(Atom "base"), (Atom "power")]) $ do
    b <- toFloat 0 exptFunc
    p <- toFloat 1 exptFunc 
    return $ tryToInt (b ** p)

sqrtFunc :: Function
sqrtFunc = Internal "sqrt" (ReqArgs [(Atom "number")]) $ toFloat 0 sqrtFunc >>= return.tryToInt.sqrt

incfFunc :: Function
incfFunc = Internal "incf" (OptArgs [(Atom "place")] [(Atom "delta")]) $ do
    b <- toFloat 0 incfFunc 
    p <- option 1.0 $ toFloat 1 incfFunc 
    return $ tryToInt (b + p)

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

tryToInt x = if isInt x 
                then Integer (floor x)
                else Float x

lispToFloat lisp = case lisp of 
                    Float f   -> f
                    Integer i -> fromInteger i
{--
--calcPrecision :: Num a => [Lisp] -> Lisp -> (a -> a -> a) -> LParser Lisp
calcPrecision list start func = foldM (apFunc func) start list
    where apFunc func acc num = 
            case acc of 
                Integer a -> case num of 
                                 Integer b -> return $ Integer (func a b)
                                 Float   b -> return $ Float (func (fromInteger a) b)
                                 _         -> fail $ "Error in function: Cannot apply a mathematical function to something not a number"
                Float a -> case num of 
                                 Integer b -> return $ Float (func a (fromInteger b))
                                 Float   b -> return $ Float (func a b)
                                 _         -> fail $ "Error in function: Cannot apply a mathematical function to something not a number"
                                 

--}

plusFunc :: Function
plusFunc = Internal "+" (RestArgs [] (Atom "numbers")) $ do 
    list <- isNumList 0 plusFunc
    foldM pls (Integer 0) list 
    where pls acc num = 
            case acc of 
                Integer a -> case num of 
                                 Integer b -> return $ Integer (a + b)
                                 Float   b -> return $ Float ((fromInteger a) + b)
                                 _         -> fail $ "Error in function " ++ (atom plusFunc) ++ ": Cannot add something not a number"
                Float a -> case num of 
                                 Integer b -> return $ Float (a + (fromInteger b))
                                 Float   b -> return $ Float (a + b)
                                 _         -> fail $ "Error in function " ++ (atom plusFunc) ++ ": Cannot add something not a number"
    
minusFunc :: Function 
minusFunc = Internal "-" (RestArgs [(Atom "number")] (Atom "more-numbers")) $ do
    num  <- isNumber 0 minusFunc      
    list <- isNumList 1 minusFunc
    foldM minus num list
    where minus acc num = 
            case acc of 
                Integer a -> case num of 
                                 Integer b -> return $ Integer (a - b)
                                 Float   b -> return $ Float ((fromInteger a) - b)
                                 _         -> fail $ "Error in function " ++ (atom minusFunc) ++ ": Cannot subtract something not a number"
                Float a -> case num of 
                                 Integer b -> return $ Float (a - (fromInteger b))
                                 Float   b -> return $ Float (a - b)
                                 _         -> fail $ "Error in function " ++ (atom minusFunc) ++ ": Cannot subtract something not a number"

multFunc :: Function
multFunc = Internal "*" (RestArgs [] (Atom "numbers")) $ do 
    list <- isList 0 multFunc
    foldM mul (Integer 1) list
    where mul acc num = 
            case acc of 
                Integer a -> case num of 
                                 Integer b -> return $ Integer (a * b)
                                 Float   b -> return $ Float ((fromInteger a) * b)
                                 _         -> fail $ "Error in function " ++ (atom multFunc) ++ ": Cannot multiply something not a number"
                Float a -> case num of 
                                 Integer b -> return $ Float (a * (fromInteger b))
                                 Float   b -> return $ Float (a * b)
                                 _         -> fail $ "Error in function " ++ (atom multFunc) ++ ": Cannot multiply something not a number"
    
divideFunc :: Function 
divideFunc = Internal "/" (RestArgs [(Atom "number")] (Atom "more-numbers")) $ do
    num  <- (isInteger 0 divideFunc >>= return . fromInteger) 
              <|> isFloat 0 divideFunc 
    list <- isList 1 divideFunc
    res <- foldM div num list
    return $ case isInt res of 
                True  -> Integer $ toInteger $ round res
                False -> Float res
    where div acc num = case num of 
                             Integer 0 -> fail $ "Error in function " ++ (atom divideFunc) ++ ": Cannot divide by zero"
                             Float 0.0 -> fail $ "Error in function " ++ (atom divideFunc) ++ ": Cannot divide by zero"
                             Integer b -> return (acc / (fromInteger b))
                             Float   b -> return (acc / b)
                             _         -> fail $ "Error in function " ++ (atom divideFunc) ++ ": Cannot divide something not a number"


regexFunc :: Function 
regexFunc = Internal "regex" (ReqArgs [(Atom "pat"), (Atom "str")]) $ do
        pat <- isString 0 regexFunc 
        str <- isString 1 regexFunc
        let res = getAllTextMatches $ str =~ pat :: [String]  
        return $ List $ map (\x -> String x) res

carFunc :: Function
carFunc = Internal "car" (ReqArgs [(Atom "list")]) $ isList 0 carFunc >>= return.head

cdrFunc :: Function
cdrFunc = Internal "cdr" (ReqArgs [(Atom "list")]) $ isList 0 cdrFunc >>= return . List . tail

isNil :: Int -> Function -> LParser Bool
isNil num func = do
    state <- getState
    case getAtom (nArg num func) state of
         Just Nil       -> return True
         Just (List []) -> return True
         Just _         -> return False
         Nothing        -> fail $ "Invalid argument to " ++ (atom func)

isObject :: Int -> Function -> LParser Lisp
isObject num func = do
    state <- getState 
    case getAtom (nArg num func) state of
                         Just o          -> return o
                         Nothing         -> fail $ "Invalid argument to " ++ (atom func)

isString :: Int -> Function -> LParser String
isString num func = do
    state <- getState 
    case getAtom (nArg num func) state of
                         Just (String p) -> return p
                         Just _          -> fail $ "The first argument of function " ++ (atom func) ++ " is not a string."
                         Nothing         -> fail $ "Invalid argument to " ++ (atom func)

isList :: Int -> Function -> LParser [Lisp]
isList num func = do
    state <- getState 
    case getAtom (nArg num func) state of
                         Just (List p) -> return p
                         Just _        -> fail $ "The first argument of function " ++ (atom func) ++ " is not a list."
                         Nothing       -> fail $ "Invalid argument to " ++ (atom func)

isAtom :: Int -> Function -> LParser Lisp
isAtom num func = do
    state <- getState 
    case getAtom (nArg num func) state of
                         Just (Atom  p) -> return $ Atom p
                         Just _        -> fail $ "The first argument of function " ++ (atom func) ++ " is not an atom."
                         Nothing       -> fail $ "Invalid argument to " ++ (atom func)

isFile :: Int -> Function -> LParser Lisp
isFile num func = do
    state <- getState 
    case getAtom (nArg num func) state of
                         Just (File  p) -> return $ File p
                         Just _        -> fail $ "The first argument of function " ++ (atom func) ++ " is not a file."
                         Nothing       -> fail $ "Invalid argument to " ++ (atom func)

isInteger :: Int -> Function -> LParser Integer
isInteger num func = do
    state <- getState 
    case getAtom (nArg num func) state of
                         Just (Integer p) -> return p
                         Just _        -> fail $ "The first argument of function " ++ (atom func) ++ " is not an integer."
                         Nothing       -> fail $ "Invalid argument to " ++ (atom func)

isFloat :: Int -> Function -> LParser Float
isFloat num func = do
    state <- getState 
    case getAtom (nArg num func) state of
                         Just (Float p) -> return p
                         Just _        -> fail $ "The first argument of function " ++ (atom func) ++ " is not a float."
                         Nothing       -> fail $ "Invalid argument to " ++ (atom func)

isNumber :: Int -> Function -> LParser Lisp
isNumber num func = 
    (try (isInteger num func) >>= return.Integer) 
        <|> (isFloat num func >>= return.Float)

isNumList :: Int -> Function -> LParser [Lisp]
isNumList num func = do 
    list <- isList num func 
    mapM isNum list 
    where isNum v = case v of 
                    Integer a -> return $ Integer a
                    Float a   -> return $ Float a
                    a         -> fail $ "Error: Unexpected value " ++ show a ++ " in a function that operate only on numbers."
                    

toFloat :: Int -> Function -> LParser Float 
toFloat num func = try (isInteger num func >>= return.fromInteger) <|> isFloat num func 

isBool :: Int -> Function -> LParser Bool
isBool num func = do
    state <- getState 
    case getAtom (nArg num func) state of
                         Just Nil      -> return False 
                         Just T        -> return True 
                         Just _        -> fail $ "The first argument of function " ++ (atom func) ++ " is not a boolean value."
                         Nothing       -> fail $ "Invalid argument to " ++ (atom func)

concatLisp list = foldr1 (<>) list

notFunc :: Function
notFunc = Internal "not" (ReqArgs [(Atom "obj")]) $ do
    obj <- isObject 0 notFunc
    return $ case obj of 
                Nil        -> T
                _          -> Nil 

mapcarFunc :: Function
mapcarFunc = Internal "mapcar" (RestArgs [(Atom "function"), (Atom "list")] (Atom "more-lists")) $ do
    state     <- getState
    Atom func <- isAtom 0 mapcarFunc
    list1     <- isList 1 mapcarFunc
    List list <- try ((isList 2 mapcarFunc) >>= \rest-> return $ (concatLisp $ list1 ++ rest)) <|> return (List list1)
    eachF (functions state) func list
    where
        eachF []     fname list = fail $ "Unknown function call: " ++ fname 
        eachF (f:fs) fname list = 
            if atom f == fname
                then do 
                    res <- mapM (\l -> parseWhile f (defArgs f l)) list
                    return $ List res
                else eachF fs fname list
        defArgs f v = 
            case args f of 
                ReqArgs  a    -> [(head a, v)]
                RestArgs a _  -> [(head a, v)]   
                OptArgs a _   -> [(head a, v)]
                         
loadFunc :: Function
loadFunc = Internal "load" (ReqArgs [(Atom "file")]) $ do
    path  <- isString 0 loadFunc 
    res   <- liftIO $ doesFileExist path 
    case res of 
         True  -> do 
            ret <- getInput 
            source <- liftIO $ readFile path
            setInput source 
            lisp <- parseLisp
            setInput ret
            return lisp
         False -> fail $ "File called by " ++ (atom loadFunc) ++ " does not exist."

appendFunc :: Function
appendFunc = Internal "append" (RestArgs [] (Atom "lists")) $ do
    lists <- isList 0 appendFunc   
    return $ concatLisp lists

pushFunc :: Function
pushFunc = Internal "push" (ReqArgs [(Atom "obj"), (Atom "place")]) $ do
    state <- getState
    obj <- case getAtom (firstArg pushFunc) state of
                Just  a     -> return a
                Nothing     -> fail $ "Invalid argument to " ++ (atom pushFunc)
    list <- isList 1 pushFunc 
    return $ List (obj:list)

lengthFunc :: Function 
lengthFunc = Internal "length" (ReqArgs [(Atom "list")]) $ do
    list <- isList 0 lengthFunc
    return $ Integer $ toInteger $ length list 

probefileFunc :: Function
probefileFunc = Internal "probe-file" (ReqArgs [(Atom "file")]) $ do 
    path  <- isString 0 probefileFunc
    res   <- liftIO $ doesFileExist path 
    state <- getState
    case res of 
         True  -> return $ fromJust $ getAtom (firstArg probefileFunc) state
         False -> return Nil

concatenateFunc :: Function
concatenateFunc = Internal "concatenate" (RestArgs [] (Atom "sequences")) $ do
    list <- isList 0 concatenateFunc
    return $ foldr1 (<>) list

openFunc :: Function
openFunc = Internal "open" (ReqArgs [(Atom "path")]) $ do
    state <- getState
    path  <- isString 0 openFunc
    fh    <- liftIO $ openFile path ReadWriteMode
    un    <- liftIO newUnique
    let file = File $ FileHandle (un, path)
        fileHandle = (file, fh)
    setState state{fileHandles=fileHandle:(fileHandles state)}
    return file


closeFunc :: Function
closeFunc = Internal "close" (ReqArgs [(Atom "handle")]) $ do
    state  <- getState
    handle <- isFile 0 closeFunc
    case lookup handle (fileHandles state) of
        Just h -> do liftIO $ hClose h 
                     setState state{fileHandles=(cleanHandle handle (fileHandles state) [])}
                     return T
        Nothing -> fail $ "Cannot close file: " ++ (show handle)
    where 
        cleanHandle _      []     acc = acc
        cleanHandle handle (h:hs) acc = if handle == fst h 
                                            then cleanHandle handle [] (hs ++ acc)
                                            else cleanHandle handle hs (h:acc)

readcharFunc :: Function
readcharFunc = Internal "read-char" (ReqArgs [(Atom "handle")]) $ do
    state  <- getState
    handle <- isFile 0 readcharFunc
    case lookup handle (fileHandles state) of
        Just h -> do ch <- liftIO $ tryIOError $ hGetChar h 
                     case ch of 
                         Left _ -> return Nil
                         Right c -> return $ String [c] 
        Nothing -> fail $ "Cannot close file: " ++ (show handle)

readlineFunc :: Function
readlineFunc = Internal "read-line" (ReqArgs [(Atom "handle")]) $ do
    state  <- getState
    handle <- isFile 0 readcharFunc
    case lookup handle (fileHandles state) of
        Just h -> do ch <- liftIO $ tryIOError $ hGetLine h 
                     case ch of 
                         Left _ -> return Nil
                         Right l -> return $ String l 
        Nothing -> fail $ "Cannot close file: " ++ (show handle)

printFunc :: Function 
printFunc = Internal "print" (OptArgs [(Atom "object")] [(Atom "stream")]) $ do
    obj <- isObject 0 printFunc
    liftIO $ putStrLn $ show obj
    return $ String $ show obj

nullFunc :: Function 
nullFunc = Internal "null" (ReqArgs [(Atom "object")]) $ isNil 0 nullFunc >>= return.toLisp

randomFunc :: Function
randomFunc = Internal "random" (ReqArgs [(Atom "limit")]) $ do
    try (isInteger 0 randomFunc >>= getRand >>= return.Integer) 
        <|> (isFloat 0 randomFunc >>= getRand >>= return.Float)
    where getRand lim = liftIO $ randomRIO (0, lim) 

parseIf :: LParser Lisp
parseIf = do
    istring "if" >> spaces 
    res <- parseExpr  
    spaces
    case res of 
        T   -> do 
            res <- parseExpr
            try spaces >> try skipExpr
            return res
        Nil  -> skipExpr >> try spaces >> try parseExpr
        _           -> fail "Condition statement in 'if' call must resolve to a boolean value."

setParseTrue :: LParser ()
setParseTrue = getState >>= \state -> setState state{ shouldParse=(True, Nil) }

setParseFalse :: Lisp -> LParser ()
setParseFalse retval = getState >>= \state -> setState state{ shouldParse=(False, retval) }

parseLoop :: LParser Lisp 
parseLoop = do
    istring "loop" >> spaces 
    state <- getState 
    setState $ addFunc returnFunc state
    for <- try $ optionMaybe $ parseFor
    ret <- case for of 
            Just a  -> (try (parseIn a) <|> parseFrom a) >>= return.List
            Nothing -> do 
                back <- getInput 
                r <- forever $ do
                        d   <- try $ optionMaybe $ (spaces >> istring "do" >> spaces)
                        res <- case d of 
                                Just _  -> parseExpr `sepBy` spaces >>= return.last
                                Nothing -> parseExpr
                        setInput back
                        return $ res 
                return $ List r
    setParseTrue
    setState $ removeFunc returnFunc state
    return $ ret
    where 
        returnFunc = Internal "return" (ReqArgs [(Atom "retval")]) $ do
            state  <- getState 
            retval <- isObject 0 returnFunc
            setParseFalse retval
            return retval
        mapMTimes beg var [] acc = return acc
        mapMTimes beg var (l:ls) acc = do
            state <- getState 
            case (shouldParse state) of 
                (True, _)  -> withAtoms [(var, l)] $ do 
                                    setInput beg 
                                    res <- parseExpr `sepBy` spaces 
                                    mapMTimes beg var ls (acc ++ [(last res)])
                (False, v) -> mapMTimes beg var [] [v] 
        parseFor = do
            istring "for" >> spaces 
            a <- parseAtom 
            spaces 
            return a
        parseFrom a = do
            istring "from" >> spaces 
            beg  <- parseInteger >>= (\(Integer i) -> return $ fromInteger i :: LParser Int)
            spaces >> istring "to" >> spaces 
            end  <- parseInteger >>= (\(Integer i) -> return $ fromInteger i :: LParser Int)
            spaces >> istring "do" >> spaces
            back <- getInput 
            mapMTimes back a (map (\i -> Integer (toInteger i)) (take (end - beg + 1) [beg..])) []
        parseIn a = do
            istring "in" >> spaces 
            list <- parseExpr
            spaces 
            case list of 
                List lst -> do 
                    istring "do" >> spaces 
                    back <- getInput 
                    mapMTimes back a lst []
                _      -> fail "Function \"loop\" expects a list."

parseLambda :: LParser Lisp
parseLambda = do
    istring "lambda" >> spaces 
    ars   <- sexp $ parseAtom `sepBy` spaces
    sp    <- spaces >> skipSexp
    state <- getState
    setState $ addFunc (External "lambda" (ReqArgs ars) sp) state
    return $ Atom "lambda"

parseSetq :: LParser Lisp
parseSetq = do
    istring "setq" >> spaces 
    at  <- parseAtom 
    spaces 
    val <- parseExpr
    old <- getState
    setState $ defineAtoms [(at, val)] $ dropAtoms [(at, val)] old
    return val

parseLet :: LParser Lisp
parseLet = do
    istring "let" >> spaces
    lets <- sexp $ letSexp `sepBy` spaces
    spaces 
    res  <- withAtoms lets (parseExpr `sepBy` spaces)
    return $ last res
    where  
        letSexp = sexp $ do
            at  <- parseAtom
            spaces 
            val <- parseExpr
            return (at, val)

parseCase :: LParser Lisp
parseCase = do 
    istring "case" >> spaces 
    obj <- parseExpr 
    spaces
    nextSexp obj
    where 
        nextSexp obj = do res <- parseTest obj
                          try spaces
                          case res of 
                            Nothing -> (nextSexp obj) <|> return Nil
                            Just x  -> do try $ skipExpr `sepBy` spaces >> return x 
        parseTest obj = sexp $ do 
            test <- parseTypes
            try spaces 
            case test of
                List l -> if any (== obj) l 
                            then parseTrue
                            else parseFalse
                Atom "otherwise" -> parseTrue
                a      -> if obj == a 
                            then parseTrue
                            else parseFalse
                where parseFalse = skipExpr >> return Nothing
                      parseTrue  = parseExpr >>= return.Just 

parseCond :: LParser Lisp
parseCond = istring "cond" >> spaces >> nextSexp 
    where
        nextSexp = do res <- condSexp 
                      try spaces
                      case res of 
                        Nothing -> nextSexp <|> return Nil
                        Just x  -> do try $ skipExpr `sepBy` spaces >> return x 
        condSexp = sexp $ do
            test <- parseExpr
            try spaces 
            case test of
                T          -> parseTrue
                Nil        -> parseFalse
                _          -> parseTrue
                where parseFalse = skipExpr  >> return Nothing
                      parseTrue  = parseExpr >>= return.Just


parseComment :: LParser Lisp
parseComment = between (char ';') (string "\n") (many (noneOf "\n")) >>= return.Comment

parseList :: LParser Lisp
parseList = do 
    x <- sexp $ parseTypes `sepBy` spaces 
    return $ List x

parseAList = do 
    list <- sexp $ parseElem `sepBy` spaces
    return $ AList list
    where parseElem = sexp $ do 
            a1 <- parseTypes
            spaces 
            a2 <- parseTypes 
            return (a1, a2)

parseQuoted :: LParser Lisp
parseQuoted = char '\'' >> parseTypes 

parseString :: LParser Lisp
parseString = between (char '"') (char '"') (many (noneOf "\"")) >>= return.String

parseInteger :: LParser Lisp
parseInteger = try $ do
    neg <- option ' ' $ char '-'
    dig <- many1 digit
    notFollowedBy $ char '.'
    return $ Integer $ read (neg : dig)

parseFloat :: LParser Lisp
parseFloat  = do
    neg <- option ' ' $ char '-'
    dig <- try $ do
        res <- many1 digit
        char '.'
        return res
    dec <- many1 digit
    return $ Float (read (neg : dig ++ "." ++ dec))

parseTypes :: LParser Lisp 
parseTypes = try parseAList
         <|> parseList  
         <|> parseString 
         <|> parseInteger 
         <|> parseFloat
         <|> parseQuoted
         <|> parseAtom
         
parseBoundAtom :: LParser Lisp
parseBoundAtom = do 
    at <- parseAtom 
    case at of 
        Nil    -> return Nil 
        T      -> return T
        _      -> do
            s <- getState
            case getAtom at s of
                Nothing -> fail $ "The variable " ++ show at ++ " is unbound"
                Just j  -> return j

parseExpr :: LParser Lisp 
parseExpr = do 
        state <- getState
        case shouldParse state of 
            (True, _) -> parseSExpr
                     <|> parseBoundAtom
                     <|> parseQuoted
                     <|> parseTypes 
            (False,_) -> skipExpr >> return ((\(_, v) -> v) (shouldParse state))

parseAtomAsFunc :: LParser Lisp 
parseAtomAsFunc = do 
    at <- parseBoundAtom 
    spaces
    state <- getState
    case getFuncByAtom at state of 
        Just f  -> parseArgs f >>= parseWhile f
        Nothing -> fail "Bound atom called as unknown function"

parseSExpr :: LParser Lisp
parseSExpr = sexp $ try parseCond
                <|> try parseCase
                <|> try parseIf
                <|> try parseSetq
                <|> try parseLet
                <|> try parseLambda
                <|> try parseLoop
                <|> try parseAtomAsFunc                 
                <|> (getState >>= eachF.functions) 
            where 
                eachF []     = fail "Unknow function call"
                eachF (f:fs) = (try.parseFuncCall) f <|> eachF fs 

parseLisp :: LParser Lisp
parseLisp = do 
    res <- (parseComment 
            <|> try parseDefun 
            <|> parseExpr 
            <?> "Unknown invalid syntax" ) `endBy` spaces
    return $ case res of
                []   -> Nil
                a:[] -> a
                _    -> List res
        

returnLisp :: LParser (Lisp, LispState)
returnLisp = do 
    lisp  <- parseLisp 
    state <- getState
    return (lisp, state)
        
runLispScript :: FilePath -> LispState -> IO (Maybe (Lisp, LispState))
runLispScript input state = do
     res <- readFile input >>= runParserT returnLisp state input 
     case res of 
        Left e      -> putStrLn "Error in LispScript: " >> print e >> return Nothing
        Right val   -> return $ Just val

isComment :: Lisp -> Bool
isComment (Comment _) = True
isComment _           = False

remComments :: Lisp -> Lisp
remComments (List lisp) = List $ filter (not.isComment) lisp
