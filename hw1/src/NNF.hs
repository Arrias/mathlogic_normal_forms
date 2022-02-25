module NNF where
import SF
import Formula

-- to NNF simple formula
toNNFNorm :: Formula -> Formula
toNNFNorm f = case f of 
                    Atom s -> Atom s
                    x `Or` y -> (toNNF x) `Or` (toNNF y)
                    x `And` y -> (toNNF x) `And` (toNNF y)
                    Not (Not f) -> toNNF f
                    Not (Atom s) -> Not (Atom s)
                    Not (x `Or` y) -> (toNNF $ Not x) `And` (toNNF $ Not y)
                    Not (x `And` y) -> (toNNF $ Not x) `Or` (toNNF $ Not y)

toNNF = toNNFNorm . simplify

