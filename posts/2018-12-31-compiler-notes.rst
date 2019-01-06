---
title: Notes on Compiler Construction
---

Disclaimer: this is my personal note, which can *possibly* have O(N) errors.

Lexical Analysis
================
A lexer is not necessary but beneficial for a compiler frontend. A lexer works on *regular languages*, transforming
character streams into classified token streams. With a lexer, the parser no longer needs to care about annoying details
such as whitespaces and identifiers. There are many existing tools for lexer generation, e.g. ``flex`` on UNIX. However
for performance consideration, lexers and parsers are often hand-written.

The theory behind a lexer is finite state automata. The construction of a lexer can be separated into several phases:

- Generate a *non-deterministic finite automaton*(NFA) based on the regular language.
- Convert the NFA into a *deterministic finite automaton* using powerset construction.
- Minimize the DFA. A typical algorithm is table filling, i.e. Myhill Nerode theorem.

One can also directly generate the DFA from the regular language using `Brzozowski derivatives<http://matt.might.net/articles/implementation-of-regular-expression-matching-in-scheme-with-derivatives/>`_, which is algebraic and elegant.


Parsing
=======
Parsing transforms token streams into parse trees. To do parsing on a language, formal definitions are required for
these languages. *Context free grammers*(CFG) are thus introduced to formally define context free languages. According
to the Chomsky hierarchy, there are four classes on languages: recursively enumerable, context sensitive, context free
and regular. Generally, the *word problem* (whether a string conforms to a grammer) is undecidable. But it can be
solved efficiently with constraints.

Ambiguity can happen in parsing and the problem is two-fold: derivations with different orders and ambiguity in the grammer itself.
For the first issue, we can replace parse trees with abstract syntax trees. For the ambiguity in the grammer itself,
we can rewrite the production rules to reflect precedence and associativity.

Several parsing methods:

- CYK parsing: iteratively apply production rules on substrings till saturation. The language should be defined in
  Chomsky normal form. The running time of this algorithm is O(N^3). (`animation<https://www.xarg.org/tools/cyk-algorithm/>`_)
- Recursive-descent parsing(predictive parsing): parses top-down. This algorithm only works when the first terminal symbol
  in each production rules gives enough information. Some techniques to enforce this are left recursion elimination and
  left factoring. Some information can be calcuated to help us build a predictive parser using ``FIRST``, ``FOLLOW`` and
  ``nullable`` sets, and these sets are also computed iteratively till saturation. If the grammer itself is left-recursive
  by nature, then some kind of workaround is required. A possible solution is to generate all possible parse trees for
  the input source code, and then eliminate invalid parse trees in later phases. Another solution is to apply lookaheads,
  also known as LL(k), which is left-to-right, left-most derivation with k-token lookahead. Recursive descent parsers are
  often taken by mainstream compilers due to its flexibility and user-friendly error reporting.
- Shift-reduce parsing: parses bottom up, also known as LR(k), which is left-to-right, right-most derivation parsing with
  k-token lookahead. This approach is often used for parser generator tools such as yacc due to the fact that LR grammers
  are a superset of LL grammers. Shift-reduce parsers can also be viewed as automata with a stack, i.e. pushdown automata.
  LR(0): LR parser with no lookahead, the key operations are CLOSURE and GOTO; SLR: LR(0) with FOLLOW set(slightly better
  for reduce); LR(1): LR(0) with one symbol lookahead; LALR(1): merged version of LR(1).

N.B. all of the above-mentioned parsing algorithms require the grammer to be unambiguous. Therefore, it is the language
spec authors' responsibility to keep the language spec unambiguous. Typical examples are the matched "else" issue and
operator precedence parsing.

Resouces:

- `Parsing tools in the real world<http://blog.reverberate.org/2013/09/ll-and-lr-in-context-why-parsing-tools.html>`_
- `GCC parser for c-family langs (awesome code!)<https://code.woboq.org/gcc/gcc/c/c-parser.c.html>`_
