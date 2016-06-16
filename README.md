# sphinxesc

A small module to prevent user-submitted search expressions from being 
mis-parsed into invalid Sphinx Extended Query Expressions.

## Synopsis

Example from ghci:

```
ghci> :m SphinxEscape 
ghci> putStrLn $ escapeSphinxQueryString "@tag_list hello OR quick brown fox 7/11"
@tag_list  hello | quick brown fox 7 11
ghci> 
ghci> putStrLn $ escapeSphinxQueryString "hello AND quick brown fox 7/11"
hello & quick brown fox 7 11
ghci> 

```





## Reference

* <http://sphinxsearch.com/docs/latest/extended-syntax.html> Sphinx Extended Syntax docs
