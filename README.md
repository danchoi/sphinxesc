# sphinxesc

A small module to prevent user-submitted search expressions from being 
mis-parsed into invalid Sphinx Extended Query Expressions.

## Synopsis

Example from ghci:

```
ghci> :m SphinxEscape 
ghci> putStrLn $ escapeSphinxQueryString "@tag_list hello OR quick brown fox 7/11"
@tag_list hello | quick brown fox 7 11
ghci> 
ghci> putStrLn $ escapeSphinxQueryString "hello AND quick brown fox 7/11"
hello & quick brown fox 7 11
ghci> 

```

## Explanation

sphinxesc performs very simple escaping. The abstract syntax tree it recognizes is:

```
data Expression = 
        TagFieldSearch String 
      | Literal String
      | AndOrExpr Conj Expression Expression 
  deriving Show
```

It does not parse more advanced Sphinx query expressions such as `NEAR/n`,
quorum, etc., nor does it recognize arbitrary `@field` expressions. All
non-alpha-numeric expressions are converted into whitespace or filtered out
unless they contribute to an `AND`, `OR` or `@tag_list` field search
expression. Obviously these rules are quite domain specific. The rules can be
made more configurable later.


## Testing


The command line executable `sphinxesc` can be used to test the expression parser 
and escaping of the input to the final sphinx search expression.

```
$ sphinxesc "test OR hello"
test | hello
$ sphinxesc -p "test OR hello"
Or (Literal "test") (Literal "hello")
```

There is a suite of Bash-based regression tests in `tests.txt`, where the input
is on the left, followed by `::` surrounded by any whitespace, followed by the
expected escaped output result. To run the tests, execute the script
`./test.sh`


```bash
./test.sh

INPUT                         EXPECTED                      RESULT                        PASS      
7/11                          7 11                          7 11                          PASS      
hello 7/11                    hello 7 11                    hello 7 11                    PASS      
hello OR 7/11                 hello | 7 11                  hello | 7 11                  PASS      
hello AND 7/11                hello & 7 11                  hello & 7 11                  PASS      
@tag_list fox tango 7/11      @tag_list fox tango 7 11      @tag_list fox tango 7 11      PASS      
@(tag_list) fox tango 7/11    @tag_list fox tango 7 11      @tag_list fox tango 7 11      PASS      
@(tag_list) AND               @tag_list AND                 @tag_list AND                 PASS      
```


## Reference

* <http://sphinxsearch.com/docs/latest/extended-syntax.html> Sphinx Extended Syntax docs
