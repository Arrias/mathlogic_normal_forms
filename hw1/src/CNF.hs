module CNF where
import NNF
import Formula
import DNF

swapCons :: Formula -> Formula
swapCons f = case f of 
                    p@(Atom s) -> p 
                    p@(Not f) -> p 
                    a `And` b -> (swapCons a) `Or` (swapCons b)
                    a `Or` b -> (swapCons a) `And` (swapCons b)

toCNF = swapCons . toDNF . swapCons . toNNF 
              