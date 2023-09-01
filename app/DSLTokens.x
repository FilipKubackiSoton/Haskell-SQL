{ 
module DSLTokens where 

import Data.Char
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters
$bigalpha = [A-Z]
-- big alphabetic characters

@all = \\ \' | \\ \" | ($printable # [\'\"])
@string = (\' @all* \') | (\" @all* \")


tokens :-
$white+ ; 
  "--".* ; 
  SELECT         { \p s -> TokenSelect p }
  FROM           { \p s -> TokenFrom p }
  WHERE          { \p s -> TokenWhere p }
  \,             { \p s -> TokenSeparator p }
  AS             { \p s -> TokenAs p }
  AND            { \p s -> TokenAnd p }
  OR             { \p s -> TokenOr p }
  "*"            { \p s -> TokenAll p }
  "=="           { \p s -> TokenEq p }
  "!="           { \p s -> TokenNeq p }
  ">="           { \p s -> TokenGeq p }
  "<="           { \p s -> TokenLeq p }
  ">"            { \p s -> TokenGt p }
  "<"            { \p s -> TokenLt p }
  LIKE           { \p s -> TokenLike p}
  IN             { \p s -> TokenIn p }
  UNION          { \p s -> TokenUnion p}
  JOIN           { \p s -> TokenJoin p }
  MERGE          { \p s -> TokenMerge p }
  ON             { \p s -> TokenOn p }
  CROSS          { \p s -> TokenCross p}
  PRESERVE       { \p s -> TokenPreserve p}
  ORDER          { \p s -> TokenOrder p}
  BY             { \p s -> TokenBy p}
  NOT            { \p s -> TokenNot p}
  \(             { \p s -> TokenLParen p }
  \)             { \p s -> TokenRParen p }
  \[             { \p s -> TokenLBrack p }
  \]             { \p s -> TokenRBrack p }
  FILE\(@string\)   { \p s -> TokenFile p $ slice 6 (length s - 2) s }
  FILE\([$alpha $digit \_ \’ \.]+\) { \p s -> TokenFile p $ slice 5 (length s - 1) s }
  NULL           { \p s -> TokenNull p }
  \$ [$alpha $digit \_ \’ \.]*   { \p s -> TokenVar p s }
  $digit+.$digit+ { \p s -> TokenFloat p (read s) }
  $digit+       { \p s -> TokenInt p (read s) }
  @string      { \p s -> TokenString p $ unescape (slice 1 (length s - 1) s) }

  

{ 
-- Each action has type :: AlexPosn -> String -> DSLToken 

-- The token type: 
data DSLToken = 
  TokenSelect AlexPosn           |
  TokenFrom AlexPosn             |
  TokenWhere AlexPosn            |
  TokenSeparator AlexPosn        |
  TokenAs AlexPosn               |
  TokenAnd AlexPosn              |
  TokenOr AlexPosn               |
  TokenAll AlexPosn              |
  TokenEq AlexPosn               |
  TokenNeq AlexPosn              |
  TokenGeq AlexPosn              |
  TokenLeq AlexPosn              |
  TokenGt AlexPosn               |
  TokenLt AlexPosn               |
  TokenLike AlexPosn             |
  TokenIn AlexPosn               |
  TokenUnion AlexPosn             |
  TokenJoin AlexPosn             |
  TokenMerge AlexPosn            |
  TokenOn AlexPosn               |
  TokenPreserve AlexPosn         |
  TokenOrder AlexPosn            |
  TokenBy AlexPosn               |
  TokenNot AlexPosn              |
  TokenCross AlexPosn            |
  TokenLParen AlexPosn           |
  TokenRParen AlexPosn           |
  TokenLBrack AlexPosn           |
  TokenRBrack AlexPosn           |
  TokenFile AlexPosn String      |
  TokenNull AlexPosn             | 
  TokenVar AlexPosn String       |
  TokenString AlexPosn String    |
  TokenInt AlexPosn Int          |
  TokenFloat AlexPosn Float
  deriving (Eq,Show) 

tokenPosn :: DSLToken -> String
tokenPosn (TokenSelect (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFrom (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhere (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSeparator (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAs (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAll (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNeq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGeq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLeq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLike (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIn (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenUnion (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenJoin (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMerge (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPreserve (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOrder (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBy (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOn (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCross (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLBrack (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRBrack (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFile (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFloat (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)

slice :: Int -> Int -> [a] -> [a]
slice a b = (take (b - a)) . (drop a)

unescape :: String -> String
unescape [] = []
unescape xs = x':xs' where
    [(x', xs'')] = readLitChar xs
    xs' = unescape xs''
}
