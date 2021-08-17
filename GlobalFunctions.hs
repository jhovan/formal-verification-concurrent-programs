module GlobalFunctions (replaceInAssocL, addToAssocL, addOrRepAssocL, keyInAssocL, setElem, remFileIfExists, openInputFile, readElse, showSepList, putStringList, tabK, tab4, pcStatic)
-- 
-- mcb
--
where
-------------------------------------------------------------------------------
--
--import Data.List (delete) -- nub
--import GlobalTypes ()
import System.Directory (removeFile,doesFileExist)
import Text.Read (readMaybe)
import System.IO (IOMode(ReadMode),Handle, openFile, stdin) --,hClose
--
--
-------------------------------------------------------------------------------
--
-- Functions for association list: --------------------------------------------
--
keyInAssocL :: Eq a => a -> [(a, t)] -> Bool
keyInAssocL key assocL = key `elem` [k | (k,_) <- assocL]
--
replaceInList :: Eq t => t -> t -> [t] -> [t]
-- Replace x with xNew in a list tL
replaceInList x xNew tL = map (\h -> if h == x then xNew else h) tL
--
replaceInAssocL :: (Eq k, Eq v, Show k) => (k, v) -> [(k, v)] -> [(k, v)]
--Replace (key,newVal) in an association list
replaceInAssocL (key,newVal) assocL = 
    case keyVal of
        Just val    -> replaceInList (key,val) (key,newVal) assocL
        Nothing     -> error $ "replaceInAssocL: key not in assocList: "++ show key 
    where
    keyVal = lookup key assocL
--
addToAssocL :: (Eq k, Eq v, Show k, Show v) => (k, v) -> [(k, v)] -> [(k, v)]
--Add (key,newVal) to the end of an association list
addToAssocL (key,newVal) assocL = 
    case keyVal of
        Nothing     -> assocL ++ [(key,newVal)]
        Just val    -> error $ "addToAssocL: (key,_) is already in assocList: "++ show (key,val)
    where
    keyVal = lookup key assocL
--
addOrRepAssocL :: (Eq k,Eq v) => (k,v) -> [(k, v)] -> [(k, v)]
--Add or replace (key,newVal) in an association list
addOrRepAssocL (key,newVal) assocL = 
    case keyVal of
        Nothing     -> assocL ++ [(key,newVal)]
        Just val    -> replaceInList (key,val) (key,newVal) assocL
    where
    keyVal = lookup key assocL
--
-------------------------------------------------------------------------------
setElem :: Int -> [a] -> a -> [a]
-- Set the nth element of a list (counting from 1) to newElem
setElem nth list newElem 
    | 0 < nth && nth <= length list = ltN ++ newElem : tail geqN
    | otherwise                     = error $ "setElem: there is no position nth= "++show nth
    where
    (ltN,geqN) = splitAt (nth-1) list -- ltN: <n, geqN: >=n
--
remFileIfExists :: FilePath -> IO ()
remFileIfExists fPath =
    do
    fExists <- doesFileExist fPath
    if fExists
        then removeFile fPath
        else return ()
--
openInputFile :: FilePath -> IO Handle
openInputFile inputPath = 
    if inputPath == ""
       then -- input from standard input
            return stdin 
       else -- input from inputPath
            do
            fHandle <- openFile inputPath ReadMode 
            return fHandle
--
-------------------------------------------------------------------------------
--
readElse :: Read a => String -> a -> a
readElse s d = case readMaybe s of
                Just r  -> r
                Nothing -> d -- default
--
-------------------------------------------------------------------------------
--
showSepList :: Show a => String -> String -> [a] -> String
--Show elements of list xL indented by ind and separated by sep
showSepList ind sep xL = 
    case xL of
        []    -> ""
        [x]   -> ind ++ show x ++ sep
        x:lx  -> ind ++ show x ++ sep ++ showSepList ind sep lx
--
--Test:
--putStrLn $ showSepList "\t" ",\n" ["a","b","c","d"] 
--
putStringList :: [String] -> IO ()
putStringList strL = 
    case strL of
        s:l -> do{putStr s; putStringList l}
        []  -> return ()
--
--Test:
--putStringList (take 10 $ drop 0 $ haskellForSimAndSmvOf "OvenArrangement")
--putStringList (take 10 $ drop 10 $ haskellForSimAndSmvOf "OvenArrangement")
--putStringList (haskellForSimAndSmvOf "OvenArrangement")
--
--
-------------------------------------------------------------------------------
--
tabK :: Int -> String
tabK n = replicate n ' '
--
tab4 :: String
tab4 = tabK 4
--
-------------------------------------------------------------------------------
pcStatic :: Int 
-- Undefined (static) value for pc.
-- [Clarke, Model Checking 1st Ed., p. 21]: 
-- Let pc be a special variable called the program counter 
-- that ranges over the set of program labels and an additional value bottom called the undefined value. 
-- The undefined value is needed when concurrent programs are considered. 
-- In this case, pc = bottom indicates that the program is not active.
pcStatic = 0
--
-------------------------------------------------------------------------------
--
