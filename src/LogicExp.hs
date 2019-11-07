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

import           Text.Parsec hiding (parse)
import qualified Text.Parsec   as P (parse)
import Text.Parsec.Char
import Data.Either
import Data.Bifunctor
import LogicExp.Priority
import Data.List

import qualified LogicExp.Parse as LP
import LogicExp.Core
import BaseExt



unSafeParseExp s = either (error . show) id (parse s)

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
getLiteralList a = sort $ nub $ getLiteralList' a
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
partialFormulas e       = pf e
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

-------------------------------
