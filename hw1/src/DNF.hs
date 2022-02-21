module DNF where
import NNF
import Formula

toDNFNorm :: Formula -> Formula
toDNFNorm p@(Atom s) = p
toDNFNorm p@(Not f) = p 
toDNFNorm (a `Or` b) = (toDNFNorm a) `Or` (toDNFNorm b)
toDNFNorm (a `And` b) = let a_dnf = toDNFNorm a in case a_dnf of 
                                (g1 `Or` g2) -> toDNFNorm $ (g1 `And` b) `Or` (g2 `And` b)
                                otherwise -> let b_dnf = toDNFNorm b in case b_dnf of
                                    (c1 `Or` c2) -> toDNFNorm $ (c1 `And` a_dnf) `Or` (c2 `And` a_dnf)
                                    otherwise -> a_dnf `And` b_dnf

toDNF = toDNFNorm . toNNF
                 