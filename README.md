I wanted to calculate some interest rates, and then I thought — what if the type system could check if my equations make sense? So I put in place a dimension type system. I think it works!

This draft also uses [unsaturated type families], so a custom GHC is required. Although it would not be hard to replace the definitions that require this extension with something more basic.

[unsaturated type families]: https://gitlab.haskell.org/kcsongor/ghc/tree/unsaturated_type_families
