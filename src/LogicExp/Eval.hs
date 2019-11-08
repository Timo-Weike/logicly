module LogicExp.Eval
    (eval)
where

import LogicExp
import LogicExp.Valuation

eval :: LogicExp -> Valuation -> Bool
eval T _            = True
eval F _            = False
eval (Lit c) v      = getValue c v 
eval (Not a) v      = not $ eval a v
eval (And    a b) v = eval a v &&    eval b v
eval (Or     a b) v = eval a v ||    eval b v
eval (Xor    a b) v = eval a v `xor` eval b v
eval (Imp    a b) v = eval a v `imp` eval b v
eval (Equal  a b) v = eval a v ==    eval b v

xor :: Bool -> Bool -> Bool
xor a b = a /= b

imp :: Bool -> Bool -> Bool
imp a b = not a || b