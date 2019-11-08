{-# LANGUAGE FlexibleContexts #-}
module LogicExp.Parse
        ( 
            parse
            , reservedChar 
        )
where

import Prelude hiding (not, or, and, exp)
import           Text.Parsec hiding (parse)
import qualified Text.Parsec   as P (parse)
import Data.Bifunctor

import LogicExp.Core (LogicExp (..))
import BaseExt

data T =
    TLit Char 
    | TT
    | TF 
    | TAnd
    | TOr
    | TImp
    | TNot
    | TEqual
    | TXor
    | TOpen
    | TClose
    deriving (Show, Eq, Read)

braces :: String
braces = "()"
trueChar :: String
trueChar = "1tT"
falseChar :: String
falseChar = "0fF"
andOp :: String
andOp = "&^"
orOp :: String
orOp = "|vV"
notOp :: String
notOp = "-~"
impOp :: String
impOp = "=->"
eqOp :: String
eqOp = "<=->"
xorOp :: String
xorOp = "x+"
reservedChar :: String
reservedChar = concat [braces, trueChar, falseChar, andOp, orOp, notOp, impOp, eqOp, xorOp, " "]

parse :: String -> Either String LogicExp
parse str = str |> tokenize |-> nest |-> parseN
    where
        (|->) :: Either String a -> (a -> Either String b) -> Either String b
        (Right a) |-> f = f a
        (Left s) |-> _ = Left s


tokenize :: String -> Either String [T]
tokenize str = first show $ P.parse (exps <* eof) [] $ filter (/=' ') str
    where
        exps = many1 exp
        exp = try $ choice [true, false, and, or, imp, eq, xor, not, open, close, lit]
        lit = try $ TLit . head <$> stringNoneOf reservedChar
        true = try $ TT <$ stringOneOf trueChar
        false = try $ TF <$ stringOneOf falseChar
        and = try $ TAnd <$ (string "and" <|> stringOneOf andOp)
        or = try $ TOr <$ (string "or" <|> stringOneOf orOp)
        not = try $ TNot <$ (string "not" <|> stringOneOf notOp)
        imp = try $ TImp <$ choice [string "->", string "=>"]
        eq = try $ TEqual <$ choice [try $ string "<->", string "<=>"]
        xor = try $ TXor <$ choice [try $ string "xor", stringOneOf xorOp]
        open = try $ TOpen <$ char '('
        close = try $ TClose <$ char ')'

stringOneOf :: Stream s m Char => String -> ParsecT s u m String
stringOneOf s = (:[]) <$> oneOf s

stringNoneOf :: Stream s m Char => String -> ParsecT s u m String
stringNoneOf s = (:[]) <$> noneOf s

data N = 
    NList [N] 
    | NSymbol T 
    | NUOp T 
    | NBOp T deriving (Show, Eq, Read)

nest :: [T] -> Either String N
nest tss = case nestMany [] tss of
        (ns, []) -> Right $ resolvePrio $ flatten $ NList ns
        x        -> Left $ "Could not nest everything: " ++ show x 
    where
        nestMany :: [N] -> [T] -> ([N], [T])
        nestMany prev ts = case nestOne ts of
            ([], ts') -> (prev, ts')
            (ns, ts') -> nestMany (prev++ns) ts'
        
        nestOne :: [T] -> ([N], [T])
        nestOne [] = ([],[])
        nestOne (TOpen:ts) = ([NList ns], ts')
            where
                (ns,ts') = nestMany [] ts
        nestOne (TClose:ts)   = ([],ts)
        nestOne ((TLit c):ts) = ([NSymbol (TLit c)], ts)
        nestOne (TT:ts)       = ([NSymbol TT], ts)
        nestOne (TF:ts)       = ([NSymbol TF], ts)
        nestOne (TNot:ts)     = ([NUOp TNot], ts)
        nestOne (t:ts)        = ([NBOp t], ts)

flatten :: N -> N
flatten (NList []) = NList []
flatten (NList [n]) = n
flatten (NList ns)  = NList $ flatten <$> ns
flatten n           = n

parseN :: N -> Either String LogicExp
parseN (NSymbol t) = case t of
    TLit c -> Right $ Lit c
    TT     -> Right T
    TF     -> Right F
    r      -> error $ "Unexpected value for (NSymbol t): " ++ show r
parseN (NList xs) = case xs of
    [a]                -> parseN a
    [NUOp TNot, a]     -> second  Not   (parseN a)
    (NUOp TNot:a:bs)   -> parseN $ NList $ NList [NUOp TNot, a] : bs
    (a:NBOp TAnd:   b) -> combine And   (parseN a) (parseN $ NList b)
    (a:NBOp TOr:    b) -> combine Or    (parseN a) (parseN $ NList b)
    (a:NBOp TXor:   b) -> combine Xor   (parseN a) (parseN $ NList b)
    (a:NBOp TImp:   b) -> combine Imp   (parseN a) (parseN $ NList b)
    (a:NBOp TEqual: b) -> combine Equal (parseN a) (parseN $ NList b)
    r                  -> Left $ "Unresolveable patter: " ++ show r
parseN r = Left $ "Could not parse syntax tree " ++ show r

combine :: (a -> b -> c) -> Either String a -> Either String b -> Either String c
combine _ (Left s)  (Left r)  = Left $ s ++ "/" ++ r
combine _ (Left s)  _         = Left s
combine _ _         (Left r)  = Left r
combine f (Right a) (Right b) = Right $ f a b

resolvePrio :: N -> N
resolvePrio (NList []) = NList []
resolvePrio (NList [x]) = NList [x]
resolvePrio (NList xs) = flatten $ NList $ nestPrio xs
    where
        nestPrio a = 
            a 
            |> nestNot 
            |> nestAnd
            |> nestOr
            |> nestXor
            |> nestImp
            |> nestEqual
resolvePrio n = n 

-- prio is not and or xor imp eq 

nestRec :: ([N] -> [N]) -> N -> N
nestRec f a = case a of
    NList as -> flatten $ NList $ f as
    r        -> r

nestNot :: [N] -> [N]
countNots :: (N -> N) -> [N] -> [N]
nestAnd :: [N] -> [N]
nestOr :: [N] -> [N]
nestXor :: [N] -> [N]
nestImp :: [N] -> [N]
nestEqual :: [N] -> [N]

nestNot [] = []
nestNot ns = let
    ns' = nestRec nestNot <$> ns
    ns'' = countNots id ns'
    in
        ns''

countNots _ [] = []
countNots f (NUOp TNot:ns) = countNots f' ns
    where
        f' n = f $ NList [NUOp TNot, n]
countNots f (n:ns) = f n : countNots id ns


nestAnd [] = []
nestAnd [a,NBOp TAnd,b]    = [NList [nestRec nestAnd a, NBOp TAnd, nestRec nestAnd b]]
nestAnd (a:NBOp TAnd:b:cs) = [NList $ nestAnd $ NList [a',NBOp TAnd,b'] : cs]
    where
        a' = nestRec nestAnd a
        b' = nestRec nestAnd b
nestAnd (a:bs)             = nestRec nestAnd a : nestAnd bs

nestOr [] = []
nestOr [a,NBOp TOr,b]    = [NList [nestRec nestOr a, NBOp TOr, nestRec nestOr b]]
nestOr (a:NBOp TOr:b:cs) = [NList $ nestOr $ NList [a',NBOp TOr,b'] : cs]
    where
        a' = nestRec nestOr a
        b' = nestRec nestOr b
nestOr (a:bs)            = nestRec nestOr a : nestOr bs 

nestXor [] = []
nestXor [a,NBOp TXor,b]    = [NList [nestRec nestXor a,NBOp TXor,nestRec nestXor b]]
nestXor (a:NBOp TXor:b:cs) = [NList $ nestXor $ NList [a',NBOp TXor,b'] : cs]
    where
        a' = nestRec nestXor a
        b' = nestRec nestXor b
nestXor (a:bs)             = nestRec nestXor a : nestXor bs

nestImp [] = [] 
nestImp [a,NBOp TImp,b]    = [NList [nestRec nestImp a,NBOp TImp,nestRec nestImp b]]
nestImp (a:NBOp TImp:b:cs) = [NList $ nestImp $ NList [a',NBOp TImp,b'] : cs]
    where
        a' = nestRec nestImp a
        b' = nestRec nestImp b
nestImp (a:bs)             = nestRec nestImp a : nestImp bs

nestEqual [] = []
nestEqual [a,NBOp TEqual,b]    = [NList [nestRec nestEqual a,NBOp TEqual,nestRec nestEqual b]]
nestEqual (a:NBOp TEqual:b:cs) = [NList $ nestEqual $ NList [a',NBOp TEqual,b'] : cs]
    where
        a' = nestRec nestEqual a
        b' = nestRec nestEqual b
nestEqual (a:bs)             = nestRec nestEqual a : nestEqual bs
