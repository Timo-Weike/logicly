module LogicExp.Valuation
    (
        Valuation
        , empty
        , getValue
        , exampleValuation
        , allValuations
        , toValueList
        , parseValuation
    )
where

import qualified Data.Map.Strict as M
import LogicExp.Parse (reservedChar)
import BaseExt

-- import for parsing
import           Text.Parsec hiding (parse)
import qualified Text.Parsec   as P (parse)

type Map = M.Map Char Bool
type Valuation = Map

empty :: Valuation
empty = M.empty

getValue :: Char -> Valuation -> Bool
getValue c v = v |> M.lookup c |> withDefault False


exampleValuation :: Valuation
exampleValuation = M.fromList [('a', True),('b', False),('c', True)]

allValuations :: String -> [Valuation]
allValuations str = tupleList
    where
        tupleList = M.fromList <$> allVals str

allVals :: String -> [[(Char, Bool)]]
allVals [] = []
allVals [c] = [[(c, False)], [(c, True)]]
allVals (c:cs) = (((c,False) :) <$> next) ++ (((c, True) :) <$> next)
    where
        next = allVals cs

toValueList :: String -> Valuation -> [Bool]
toValueList []     _ = []
toValueList (c:cs) v = getValue c v : toValueList cs v

parseValuation :: String -> Either String Valuation
parseValuation str = x
    where
        lit = try $ noneOf (':':reservedChar)
        val = try $ readBool <$> oneOf "01"
        litValPair = try $ (,) <$> lit <* char ':' <*> val
        pairList = try $ litValPair `sepBy` char ','
        maybeValList =  P.parse (pairList <* eof) [] str :: Either ParseError [(Char,Bool)]
        x = case maybeValList of
            Left s  -> Left $ show s
            Right list -> Right $ M.fromList list

readBool :: Char -> Bool
readBool '0' = False
readBool '1' = True
readBool c   = error $ "Can not convert " ++ show c ++ " to a boolean"