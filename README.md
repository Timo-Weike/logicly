# logicly

`logicly` is command line tool intended to make is easier to work with logical expressions.
One can use `logicly` to create a truth-table for a logic expression.
If you run the command `logicly -t "a | b -> b & c"`, `logicly` will produce the following output:
~~~
a b c | ((a | b) -> (b & c))
----------------------------
0 0 0 |          1          
0 0 1 |          1          
0 1 0 |          0          
0 1 1 |          1          
1 0 0 |          0          
1 0 1 |          0          
1 1 0 |          0          
1 1 1 |          1         
~~~

Since for expession with many different atomic formulas the truth-table gets bigger 
(for n different atomic formulas the table will contain 2^n rows) you can set one of the following two flags:
 * `--only-sat` print only those valuations which satisfie the expression (i.e. where the expression is 1)
 * `--only-unsat` print only those valuations which do not satisfies the expression

 Furthermore, with the flag `-e VAL` you can say logicly to evaluate the expression with a specific valuation.
 The valuation has to be in the form of `<lit>:(0|1)(,<lit>:(0|1))*` where `<lit>` is an literal.

 For further details on the api and the syntac refer to the help produced by the flag `-h`