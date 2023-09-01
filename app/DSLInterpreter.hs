module DSLInterpreter where

import DSLGrammar
import DSLTokens
import Files
import Tablefy

import Data.Typeable
import Data.List
import Text.Read
import Text.Printf
import Data.Maybe

import Text.Regex.TDFA

type Variable = (String, Attribute)
type Row = [ Variable ]

type Result = [ Row ]

-- | Interprets the query 
--
-- >>> interpret SF (Relation (Rel "A.csv"))
-- [["Haskell", "is", "great"]]
interpret :: Query -> IO(Result)
interpret (SFWP sl f w) = do
    f' <- from f
    return $ select sl $ (cond w f')
interpret (SFP sl f) = interpret (SFWP sl f (Eq (AtrI 1) (AtrI 1)))
interpret (SFW sl f w) = do
    rs <- interpret (SFWP sl f w)
    return $ sortBy (\x y -> (map showVal x) `compare` (map showVal y)) rs
interpret (SF sl f) = interpret (SFW sl f (Eq (AtrI 1) (AtrI 1)))
interpret (SFWO sl f w a) = do
    rs <- interpret (SFWP sl f w)
    return $ sortBy (\x y -> (resolveAtr a x) `compare` (resolveAtr a y)) rs
interpret (SFO sl f a) = interpret (SFWO sl f (Eq (AtrI 1) (AtrI 1)) a)

-- | Gets the minimum arity of a given list of lists
--
-- >>> len [[1, 2, 3], [4, 5, 6], [7, 8]]
-- 2
len :: [[ a ]] -> Int
len d = if length d == 0 then 0 else minimum $ map length d

-- | Pretty prints the Result
--
-- >>> pprint [[(Atr "$0", "$0 of 1st row")], [(Atr "$0", "$0 of 2nd row")]]
--     +-----------------+
--     |        $0       |
--     +-----------------+
--     |  $0 of 1st row  |
--     +-----------------+
--     |  $0 of 2nd row  |
--     +-----------------+
pprint :: Result -> String
pprint rs = tablefy (take n varNames) d where
    n = len d
    d = map (map showVal) rs

-- | Prints Result as CSV
--
-- >>> csvprint [[(Atr "$0", "$0 of 1st row")], [(Atr "$0", "$0 of 2nd row")]]
-- "$0 of 1st row\n$0 of 2nd row"
csvprint :: Result -> String
csvprint rs = intercalate "\n" (map (intercalate "," . map showVal) rs)

-- | Shows a value stored in an Attribute
-- 
-- >>> showVal (Atr "$0", "Haskell is great")
-- "Haskell is great"
showVal :: Variable -> String
showVal (_, AtrS s) = s
showVal (_, AtrI i) = show i
showVal (_, AtrF f) = show f

-- | Set of variable names
varNames :: [ String ]
varNames = map (\x -> "$" ++ show(x)) [0..]

-- | Searches a value in list of tuples by key
-- 
-- >>> search "Hello" [("Hello", "world"), ("Hi", "haskell")]
-- "world"
search :: Eq a => a -> [(a,b)] -> Maybe b
search a = fmap snd . find ((== a) . fst)

-- | Gets contents of relation as a Result
-- 
-- >>> from (Relation Rel "A.csv")
-- [[(Atr "$0", "$0 of 1st row")], [(Atr "$0", "$0 of 2nd row")]]
from :: FList -> IO (Result)
from (Relation Null) = return []
from (Relation (Rel path)) = do 
    file <- readCSVFile path
    return $ map (\l -> zip varNames $ map getAtr l) file where
        getAtr :: String -> Attribute
        getAtr a = case (readMaybe a :: Maybe Int) of
            (Just i) -> AtrI i
            Nothing -> case (readMaybe a :: Maybe Float) of
                (Just f) -> AtrF f
                Nothing -> AtrS a 
from (Cross x y) = cross (from x) (from y)
from (Union x y) = unionDSL (from x) (from y)
from (Merge x y c) = if checkJoinCond c == True 
    then merge (from x) (from y) c 
    else error "Unsupported operations under MERGE condition"
from (Join x y c) = if checkJoinCond c == True 
    then join (from x) (from y) c 
    else error "Unsupported operations under JOIN condition"
from (Relation (RRel q)) = interpret q

-- | Checks whether a condition can be applied to JOIN and MERGE
-- 
-- >>> checkJoinCond (Eq (ArgS "1") (ArgI 1))
-- True
checkJoinCond :: Cond -> Bool
checkJoinCond (Eq _ _) = True
checkJoinCond (And c d) = checkJoinCond c && checkJoinCond d
checkJoinCond _ = False

-- | Left merges @x@ and @y@ under condition @c@
merge :: IO(Result) -> IO(Result) -> Cond -> IO(Result)
merge x y c' = do
    x' <- x
    y' <- y
    let c = resolveRelative c' (len x')
    r' <- cross x y 
    let r = cond c r'
    if len x' /= len y'
        then error "Sources need to have the same arity for MERGE"
        else do return $ map selectNotEmpty r where
            selectNotEmpty r = r' where
                r' = map (\(x, y) -> if snd x == AtrS "" then y else x) $ zip (take half r) (drop half r)
                half = length r `div` 2

-- | Joins @x@ and @y@ under condition @c@
join :: IO(Result) -> IO(Result) -> Cond -> IO(Result)
join x y c' = do
    x' <- x
    let c = resolveRelative c' (len x')
    let pairs = getPairs c
    r' <- cross x y
    let r = cond c r'
    let vars = (map (\x -> Atr x) $ take (len r) varNames) \\ (map snd pairs)

    return $ select (implode vars) r

-- | Gets fields that are set equal in JOIN/MERGE condition
getPairs :: Cond -> [(Attribute, Attribute)]
getPairs (Eq a b) = [(a, b)]
getPairs (And c d) = (getPairs c) ++ (getPairs d)

-- | Resolves relative Attribute names
resolveRelative :: Cond -> Int -> Cond
resolveRelative (Eq x y) o = (Eq (resolveRelative' x o) (resolveRelative' y o)) where
    resolveRelative' :: Attribute -> Int -> Attribute
    resolveRelative' (Atr ('$':'L':'.':xs)) _ = (Atr ('$':xs))
    resolveRelative' (Atr ('$':'R':'.':xs)) o = (Atr ('$':xs')) where
        xs' = show ((read xs :: Int) + o)
    resolveRelative xs _ = xs
resolveRelative (And x y) o = (And (resolveRelative x o) (resolveRelative y o))

-- | Performs a cross of @x@ and @y@
cross :: IO(Result) -> IO(Result) -> IO(Result)
cross x y = do
    x' <- x
    y' <- y
    let x'' = map (map snd) x'
    let y'' = map (map snd) y'
    return $ map (zip varNames) [xs ++ ys | xs <- x'', ys <- y'']

-- | Performs an union of @x@ and @y@
unionDSL :: IO(Result) -> IO(Result) -> IO(Result)
unionDSL x y = do
    x' <- x
    y' <- y
    if len x' /= len y'
        then error "Sources need to have the same arity for UNION"
        else do return $ map (zip varNames) ((map (map snd) x') ++ (map (map snd) y'))

-- | Evaluates a condition
-- 
-- >>> cond (Eq (AtrS "1") (AtrI 1))
-- True
cond :: Cond -> Result -> Result
cond _ [] = []
cond c (x:xs) = (if (cond' c x) then [x] else []) ++ (cond c xs) where
    cond' :: Cond -> Row -> Bool
    cond' (And a b) vs = (&&) (cond' a vs) (cond' b vs)
    cond' (Or a b) vs = (||) (cond' a vs) (cond' b vs)
    cond' (Eq a b) vs = (==) (resolveAtr a vs) (resolveAtr b vs)
    cond' (Neq a b) vs = (/=) (resolveAtr a vs) (resolveAtr b vs)
    cond' (Geq a b) vs = (>=) (resolveAtr a vs) (resolveAtr b vs)
    cond' (Leq a b) vs = (<=) (resolveAtr a vs) (resolveAtr b vs)
    cond' (Gt a b) vs = (>) (resolveAtr a vs) (resolveAtr b vs)
    cond' (Lt a b) vs = (<) (resolveAtr a vs) (resolveAtr b vs)
    cond' (In a b) vs = if a' == Nothing 
        then False 
        else a' `elem` (map (\x -> resolveAtr x vs) $ explode b) where
            a' = resolveAtr a vs
    cond' (Like a (Pat b)) vs = case resolveAtr a vs of
        Nothing -> False
        (Just a') -> (showVal ("", a')) =~ b :: Bool
    cond' (Not c) vs = not $ cond' c vs
    
-- | Resolves attribute name to a value
-- 
-- >>> resolveAtr (Atr "$0") [(Atr "$0", AtrS "Hello")]    
-- AtrS "Hello"
resolveAtr :: Attribute -> Row -> Maybe Attribute
resolveAtr (Atr n) vs = search n vs
resolveAtr (As n (DType t)) vs 
    | t == "TEXT" = case search n vs of 
        Nothing -> Nothing
        Just x -> Just (AtrS $ showVal ("", x))
    | t == "FLOAT" = case search n vs of 
        Nothing -> Nothing
        Just x -> Just (AtrF $ toFloat (showVal ("", x)))
    | t == "INT" = case search n vs of 
        Nothing -> Nothing
        Just x -> Just (AtrI $ toInt (showVal ("", x)))
    | otherwise = error $ "Unknown datatype \'" ++ t ++ "\'"
resolveAtr a _ = Just a

-- | Selects a subset of parameters from a Result
select :: SList -> Result -> Result
select _ [] = []
select (All) rs = rs
select (AllAs (DType t)) rs 
    | t == "TEXT" = map (zip varNames . map (\x -> AtrS (showVal x))) rs
    | t == "FLOAT" = map (zip varNames . map (\x -> AtrF (toFloat $ showVal x))) rs
    | t == "INT" = map (zip varNames . map (\x -> AtrI (toInt $ showVal x))) rs
    | otherwise = error $ "Unknown datatype \'" ++ t ++ "\'"
select ss (r:rs) = (select' (explode ss) r) : (select ss rs) where
    select' :: [Attribute] -> Row -> Row
    select' as r = zip varNames (catMaybes $ map (\x -> resolveAtr x r) as)

-- | Converts SList to a list of Attributes
explode :: SList -> [Attribute]
explode (Attribute x) = [x]
explode (ArrA (x) y) = [x] ++ explode y

-- | Converts a list of Attributes to SList
implode :: [Attribute] -> SList
implode [x] = Attribute x
implode (x:xs) = ArrA x (implode xs)

-- | Converts a string to float if possible
--
-- >>> toFloat "1"
-- 1.0
toFloat :: String -> Float
toFloat s = case (readMaybe s :: Maybe Float) of
    Nothing -> case (readMaybe s :: Maybe Int) of
        Nothing -> error $ "Cannot convert \'" ++ s ++ "\' to FLOAT"
        (Just i) -> fromIntegral i :: Float
    (Just f) -> f

-- | Converts a string to int if possible
--
-- >>> toInt "1.2"
-- 1
toInt :: String -> Int
toInt s = case (readMaybe s :: Maybe Float) of
    Nothing -> case (readMaybe s :: Maybe Int) of
        Nothing -> error $ "Cannot convert \'" ++ s ++ "\' to INT"
        (Just i) -> i
    (Just f) -> floor f