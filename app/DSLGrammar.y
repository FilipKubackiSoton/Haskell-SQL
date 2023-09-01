{ 
module DSLGrammar where 
import DSLTokens 
import Data.Maybe
import Text.Read
}

%name parseDSL 
%tokentype { DSLToken } 
%error { parseError }
%token 
    SELECT      { TokenSelect _ }
    FROM        { TokenFrom _ }
    WHERE       { TokenWhere _ }
    ','         { TokenSeparator _ }
    AS          { TokenAs _ }
    AND         { TokenAnd _ }
    OR          { TokenOr _ }
    '*'         { TokenAll _ }
    '=='        { TokenEq _ } 
    '!='        { TokenNeq _ } 
    '>='        { TokenGeq _ } 
    '<='        { TokenLeq _ } 
    '>'         { TokenGt _ } 
    '<'         { TokenLt _ } 
    LIKE        { TokenLike _ }
    IN          { TokenIn _ }
    UNION       { TokenUnion _ }
    JOIN        { TokenJoin _ }
    MERGE       { TokenMerge _ }
    ON          { TokenOn _ }
    CROSS       { TokenCross _ }
    PRESERVE    { TokenPreserve _ }
    ORDER       { TokenOrder _ }
    BY          { TokenBy _ }
    NOT         { TokenNot _ }
    '('         { TokenLParen _ }
    ')'         { TokenRParen _ }
    '['         { TokenLBrack _ }
    ']'         { TokenRBrack _ }
    file        { TokenFile _ $$ }
    null        { TokenNull _}
    var         { TokenVar _ $$ }
    int         { TokenInt _ $$ }
    float       { TokenFloat _ $$ }
    string      { TokenString _ $$}

--%right SELECT
--%right FROM
--%right WHERE 
%left AND
%left OR
%left LIKE 
%left IN
%left JOIN
%left UNION
%left MERGE
%left CROSS
%left AS
%left ','
%nonassoc var '(' ')' '[' ']'
%nonassoc ON
%nonassoc PRESERVE
%nonassoc ORDER
%nonassoc BY
%nonassoc NOT

%%
Query : SELECT SList FROM FList WHERE Cond { SFW $2 $4 $6 }
      | SELECT SList FROM FList            { SF $2 $4 }
      | SELECT SList FROM FList WHERE Cond PRESERVE ORDER { SFWP $2 $4 $6 }
      | SELECT SList FROM FList PRESERVE ORDER { SFP $2 $4 }
      | SELECT SList FROM FList WHERE Cond ORDER BY Attribute { SFWO $2 $4 $6 $9 }
      | SELECT SList FROM FList ORDER BY Attribute { SFO $2 $4 $7 }

SList : Attribute ',' SList         { ArrA $1 $3 }
      | Function '(' Attribute ')'  { App $1 $3 }
      | '*' AS Type                 { AllAs $3}
      | '*'                         { All }
      | Attribute                   { Attribute $1 }

FList : Relation                    { Relation $1 }
      | FList JOIN FList            { Join $1 $3 (Eq (Atr "$L.0") (Atr "$R.0")) }
      | FList MERGE FList           { Merge $1 $3 (Eq (Atr "$L.0") (Atr "$R.0")) }
      | FList MERGE FList ON Cond   { Merge $1 $3 $5 }
      | FList JOIN FList ON Cond    { Join $1 $3 $5 }
      | FList CROSS FList           { Cross $1 $3 }
      | FList UNION FList           { Union $1 $3 }


Cond : Cond AND Cond                { And $1 $3 }
    | Cond AND '(' Cond ')'         { And $4 $1 }
    | '(' Cond ')' AND Cond         { And $2 $5 }
    | '(' Cond ')' AND '(' Cond ')' { And $2 $6 }
    | Cond OR Cond                  { Or $1 $3 }
    | Cond OR '(' Cond ')'          { Or $4 $1 }
    | '(' Cond ')' OR Cond          { Or $2 $5 }
    | '(' Cond ')' OR '(' Cond ')'  { Or $2 $6 }
    | NOT Cond                      { Not $2 }
    | Attribute '==' Attribute      { Eq $1 $3 }
    | Attribute '!=' Attribute      { Neq $1 $3 }
    | Attribute '>=' Attribute      { Geq $1 $3 }
    | Attribute '<=' Attribute      { Leq $1 $3 }
    | Attribute '>' Attribute       { Gt $1 $3 }
    | Attribute '<' Attribute       { Lt $1 $3 }
    | Attribute IN '[' SList ']'    { In $1 $4 }
    | Attribute LIKE Pattern        { Like $1  $3 }
    | Attribute NOT IN '[' SList ']'{ Not (In $1 $5) }
    | Attribute NOT LIKE Pattern    { Not (Like $1  $4) }

Attribute : var               { Atr $1 }
          | var AS Type       { As $1 $3 }
          | int               { AtrI $1 }
          | float             { AtrF $1 }
          | string            { AtrS $1 }
          | null              { AtrS "" }

Relation : var                { Rel $1 }
         | file               { Rel $1 }
         | null               { Null }
         | '(' Query ')'      { RRel $2}

Type : string                 { DType $1 }
-- we can add file parsing 

Function : var                { Fun $1 }

Pattern : string              { Pat $1 }

{ 
parseError :: [DSLToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t)) 

data Query 
    = SFW SList FList Cond
    | SF SList FList 
    | SFWP SList FList Cond
    | SFP SList FList
    | SFWO SList FList Cond Attribute
    | SFO SList FList Attribute
    deriving (Show,Eq)

data SList 
    = ArrA Attribute SList
    | App Function Attribute
    | Attribute Attribute
    | All
    | AllAs Type
    deriving (Show,Eq)           

data FList 
    = Join FList FList Cond
    | Merge FList FList Cond
    | Cross FList FList
    | Union FList FList
    | Relation Relation
    deriving (Show,Eq)                

data Cond 
    = Not Cond
    | And Cond Cond
    | Or Cond Cond
    | Eq Attribute Attribute
    | Neq Attribute Attribute
    | Geq Attribute Attribute
    | Leq Attribute Attribute
    | Gt Attribute Attribute
    | Lt Attribute Attribute
    | In Attribute SList
    | Like Attribute Pattern 
    deriving (Show,Eq)  

data Attribute = As String Type | Atr String | AtrS String | AtrI Int | AtrF Float deriving (Show)   
data Relation = Null | Rel String | RRel Query deriving (Show,Eq)   
data Function = Fun String deriving (Show,Eq)  
data Pattern = Pat String deriving (Show,Eq)  
data Type = DType String deriving (Show,Eq)  

instance Eq Attribute where 
    (==) (Atr a) (Atr b) = a == b
    (==) (Atr _) _ = False
    (==) _ (Atr _) = False
    (==) (AtrI a) (AtrI b) = a == b
    (==) (AtrF a) (AtrF b) = a == b
    (==) (AtrI a) (AtrF b) = (fromIntegral a :: Float) == b
    (==) (AtrF a) (AtrI b) = a == (fromIntegral b :: Float)
    (==) (AtrS a) (AtrS b) = a == b
    (==) (AtrS a) (AtrI b) = (readMaybe a :: Maybe Int) == (Just b)
    (==) (AtrS a) (AtrF b) = (readMaybe a :: Maybe Float) == (Just b)
    (==) (AtrI a) (AtrS b) = (Just a) == (readMaybe b :: Maybe Int)
    (==) (AtrF a) (AtrS b) = (Just a) == (readMaybe b :: Maybe Float)
    (==) _ _ = False

instance Ord Attribute where
    compare (AtrI a) (AtrI b) = a `compare` b
    compare (AtrF a) (AtrF b) = a `compare` b
    compare (AtrI a) (AtrF b) = (fromIntegral a :: Float) `compare` b
    compare (AtrF a) (AtrI b) = a `compare` (fromIntegral b :: Float)
    compare (AtrS a) (AtrS b) = a `compare` b
    compare (AtrS a) (AtrI b) = (readMaybe a :: Maybe Int) `compare` (Just b)
    compare (AtrS a) (AtrF b) = (readMaybe a :: Maybe Float) `compare` (Just b)
    compare (AtrI a) (AtrS b) = (Just a) `compare` (readMaybe b :: Maybe Int)
    compare (AtrF a) (AtrS b) = (Just a) `compare` (readMaybe b :: Maybe Float)
    compare _ _ = EQ
}
