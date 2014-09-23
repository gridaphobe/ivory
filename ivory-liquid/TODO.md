# TODO

- prove absence of overflow / underflow in IVORY code
  - want to write signatures like
    `{-@ double :: Def ('[{v:Uint8 | Btwn 0 v 128}] :-> Uint8) @-}`
  - need to
    1. parse type operators...
    2. parse type-level lists, distinguish from list type
       e.g. `'[Int,Bool]` ==> `RApp ': [Int, RApp ': [Bool, '[]]]` whereas `[Int]` ==> `RApp [] Int`
    3. make Uint8 refinement propagate down to body function
       - seems to be `true`d out atm, no clue why...
