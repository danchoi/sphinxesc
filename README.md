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

## Testing

The command line executable sphinxesc-parse can be used to test the expression parser 
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
