{-# LANGUAGE FlexibleContexts #-}
module LogicExp
    (
        LogicExp (..)
        , parse
        , toString
        , unSafeParseExp
        , getLiteralList
        , partialFormulas
    )
where

import Prelude hiding (exp)
import Data.List

import qualified LogicExp.Parse as LP
import LogicExp.Core

unSafeParseExp :: String -> LogicExp
unSafeParseExp s = either (error . show) id (parse s)

parse :: String -> Either String LogicExp
parse = LP.parse

toString :: LogicExp -> String
toString (Lit c)     = [c]
toString T           = "T"
toString F           = "F"
toString (And   a b) = "(" ++ toString a ++ " & "   ++ toString b ++ ")"
toString (Or    a b) = "(" ++ toString a ++ " | "   ++ toString b ++ ")"
toString (Imp   a b) = "(" ++ toString a ++ " -> "  ++ toString b ++ ")"
toString (Equal a b) = "(" ++ toString a ++ " <-> " ++ toString b ++ ")"
toString (Xor   a b) = "(" ++ toString a ++ " x "   ++ toString b ++ ")"
toString (Not a)     = "~" ++ toString a

getLiteralList :: LogicExp -> String
getLiteralList exp = sort $ nub $ getLiteralList' exp
    where
        getLiteralList' (Lit c)     = [c]
        getLiteralList' T           = []
        getLiteralList' F           = []
        getLiteralList' (Not a)     = getLiteralList a
        getLiteralList' (And   a b) = concatMap getLiteralList [a,b]
        getLiteralList' (Or    a b) = concatMap getLiteralList [a,b]
        getLiteralList' (Equal a b) = concatMap getLiteralList [a,b]
        getLiteralList' (Imp   a b) = concatMap getLiteralList [a,b]
        getLiteralList' (Xor   a b) = concatMap getLiteralList [a,b]

partialFormulas :: LogicExp -> [LogicExp]
partialFormulas (Lit c) = [Lit c]
partialFormulas T       = [T]
partialFormulas F       = [F]
partialFormulas exp     = pf exp
    where
        pf (Lit _)       = []
        pf T             = []
        pf F             = []
        pf e@(Not     a) = e : pf a
        pf e@(And   a b) = e : (pf a ++ pf b)
        pf e@(Or    a b) = e : (pf a ++ pf b)
        pf e@(Xor   a b) = e : (pf a ++ pf b)
        pf e@(Imp   a b) = e : (pf a ++ pf b)
        pf e@(Equal a b) = e : (pf a ++ pf b)