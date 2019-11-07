module LogicExp.Core 
    (LogicExp (..))
where

data LogicExp = 
    Lit Char
    | T
    | F
    | And LogicExp LogicExp
    | Or LogicExp LogicExp
    | Not LogicExp
    | Imp LogicExp LogicExp
    | Equal LogicExp LogicExp
    | Xor LogicExp LogicExp
    deriving (Show, Eq, Read)