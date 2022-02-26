# formula-normalizer
[![Haskell CI](https://github.com/SmnTin/simple-type-checker/actions/workflows/haskell.yml/badge.svg)](https://github.com/Arrias/mathlogic_normal_forms/actions)

### Usage
Run the following commands to build the project:
```console
$ cabal update
$ cabal build
```
Then run the program as follows:
```console
$ cabal run hw1-app <your formula>
```  
For example:
```console
$ cabal run hw1-app "a -> (!(b || c))"
NNF: ((!a) || ((!b) && (!c)))
DNF: ((!a) || ((!b) && (!c)))
CNF: (((!b) || (!a)) && ((!c) || (!a)))
```
To launch the tests run the following command(s):
```console
$ cabal test # for unit tests  
$ cabal run hw1-quick-check # for testing with quick-check lib
```
## Syntax
Variable names can contain only letters of both registers.  For example, `abcd`, `VAR`, `ILOVEmathloGic`.

### Operators

|  Op                  | Priority | Type      | Associativity    | Syntax |
|----------------------|----------|-----------|------------------|--------|
|  Not                 | 8       | Prefix    | -                | `!`    |
|  And                 | 7        | Infix     | Right             | `&&`   |
|  Or                  | 6        | Infix     | Right            | `||`   |
|  Implication         | 7        | Infix     | Right             | `->`   |
|  Double Implication  | 7        | Infix     | Right             | `<->`  |

If you want use `!!` then please **use whitespaces**, like `p1 -> ! ! p2`.
### Samples 
`p1 -> p2 -> p3` , 
`p1 || p2 && p3`,
`!p1 && !(p2 -> p3)`,
`(p1 && p2) || (p3) || !(p4) -> p5`.
