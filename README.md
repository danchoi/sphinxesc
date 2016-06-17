# sphinxesc

A small module to prevent user-submitted search expressions from being 
mis-parsed into invalid Sphinx Extended Query Expressions.

The module provides a function 

    module SphinxEscape where
    escapeSphinxQueryString :: String -> String

that sanitizes the Sphinx query expression in a way that can be safely submitted to the Sphinx API. 

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

`escapeSphinxQueryString` performs very simple escaping with the help of a
simplified abtract syntax tree. The abstract syntax tree it builds is:

```
data Expression = 
        TagFieldSearch String 
      | Literal String
      | AndOrExpr Conj Expression Expression 
  deriving Show
```

The escaping does not parse more advanced Sphinx query expressions such as
`NEAR/n`, quorum, etc., nor does it recognize arbitrary `@field` expressions.
The only special expressions recognized are `& (AND)`, `| (OR)` and `@tag_list
WORDS`.  Non-alphanumeric characters that do not form part of these specific
expressions are simply turned into whitespace. 

See the **Testing** section below for examples of conversions.

Obviously these rules are quite domain specific. The rules can be
made more configurable later.


## Testing


The command line executable `sphinxesc` can be used to test the expression parser 
and escaping of the input to the final sphinx search expression.

```
$ sphinxesc "test OR hello"
test | hello

# -p option shows the parsing result

$ sphinxesc -p "test OR hello"
AndOrExpr Or (Literal "test") (Literal "hello")
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
hello or 7/11                 hello | 7 11                  hello | 7 11                  PASS      
hello | 7/11                  hello | 7 11                  hello | 7 11                  PASS      
hello AND 7/11                hello & 7 11                  hello & 7 11                  PASS      
@tag_list fox tango 7/11      @tag_list fox tango 7 11      @tag_list fox tango 7 11      PASS      
@(tag_list) fox tango 7/11    @tag_list fox tango 7 11      @tag_list fox tango 7 11      PASS      
@(tag_list) AND               @tag_list AND                 @tag_list AND                 PASS      
@other_field AND              other field AND               other field AND               PASS      
hello & @other_field AND      hello &  other field AND      hello &  other field AND      PASS      
hello &                       hello                         hello                         PASS      
& hello &                     hello                         hello                         PASS      
& & hello &                   hello                         hello                         PASS      
| | hello |                   hello                         hello                         PASS      
"hello" hello                 hello  hello                  hello  hello                  PASS      
hello" hello                  hello  hello                  hello  hello                  PASS      
hello' hello                  hello  hello                  hello  hello                  PASS      
hello' @tag_list fox          hello   @tag_list fox         hello   @tag_list fox         PASS      
hello' @tag_list fox &        hello   @tag_list fox         hello   @tag_list fox         PASS      
                                                                                          PASS      
```

## Future directions

The escaping function can be made more configurable. The parser and AST data
structure can also be made more sophisticated, so that the AST can cover more
of the Sphinx Extended Query syntax. 

## Reference

* <http://sphinxsearch.com/docs/latest/extended-syntax.html> Sphinx Extended Syntax docs
