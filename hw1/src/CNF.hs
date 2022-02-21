module CNF where
import NNF
import Formula
import DNF

swapCons :: Formula -> Formula
swapCons p@(Atom s) = p 
swapCons p@(Not f) = p 
swapCons (a `And` b) = (swapCons a) `Or` (swapCons b)
swapCons (a `Or` b) = (swapCons a) `And` (swapCons b)

toCNF = swapCons . toDNF . swapCons . toNNF 
              