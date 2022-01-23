
### Extended2BNF

A simple converter that converts an extended grammar to a BNF grammar.

Supported constructs: one-or-more `+`, zero-or-more `*`, optional `?`, literal in non terminal rules as well as group `()`.

Input format is supposed to be in antlr style grammar, and output format is supposed to be usable in [this site](http://smlweb.cpsc.ucalgary.ca/start.html).

the `display :: Bool -> Grammar -> String` function could be triggered to revert the cases of first character in names in order to fully compatible with the 
notation used by the grammar analyzer.

#### Example

The grammar `a -> 'foo' b* (c d+ | e 'bar'+) f?.` will produce the following rules:

```
A -> A#1 A#2_st A#3_g A#4_op.
a#1 -> 'foo'.
A#3@1#2_pl1 -> D A#3@1#2_pl2.
A#3@1#2_pl2 -> A#3@1#2_pl1 | .
a#3@2#2 -> 'bar'.
A#3@2#2_pl1 -> A#3@2#2 A#3@2#2_pl2.
A#3@2#2_pl2 -> A#3@2#2_pl1 | .
A#3_g -> C A#3@1#2_pl1 | E A#3@2#2_pl1.
A#2_st ->  | B A#2_st.
A#4_op ->  | F.
```

#### Issues

- The output rules are unordered
- Naming of generated rules could be hard to read