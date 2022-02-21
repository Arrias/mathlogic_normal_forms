module NNF where
import SF
import Formula

-- to NNF simple formula
toNNFNorm :: Formula -> Formula
toNNFNorm (Atom s) = Atom s
toNNFNorm (x `Or` y) = (toNNF x) `Or` (toNNF y)
toNNFNorm (x `And` y) = (toNNF x) `And` (toNNF y)

toNNFNorm (Not (Not f)) = toNNF f
toNNFNorm (Not (Atom s)) = Not (Atom s)
toNNFNorm (Not (x `Or` y)) = (toNNF (Not x)) `And` (toNNF (Not y))
toNNFNorm (Not (x `And` y)) = (toNNF (Not x)) `Or` (toNNF (Not y))

toNNF = toNNFNorm . simplify