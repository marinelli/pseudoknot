# pseudoknot

```shell
$ stack build
...
$ cat samples/sample1.txt | stack exec pseudoknot-exe
AAGACCUGCACGCUAGUU
.(((..).(..))).(.)
$ cat samples/sample1.translation.txt | stack exec pseudoknot-strand
primary
# the primary structure is indexed starting from 1
AAGAUUUGUAUGUUAGUU
declare
# the basic loops, identified by the starting and ending indexes
h2 = (2,14), h3 = (3,13), h4 = (4,7), h9 = (9,12), h16 = (16,18)
structure
# this is an expression using the nesting and concatenation operators
((h4 + h9) | h3 | h2 + h16)
end
$ cat samples/sample1.translation.txt | stack exec pseudoknot-strand | stack exec pseudoknot-exe
AAGAUUUGUAUGUUAGUU
.(((..).(..))).(.)

```
