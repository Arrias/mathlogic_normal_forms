module DNF where
import NNF
import Formula

-- convert simplified formula to DNF

s_toDNF :: Formula -> Formula
s_toDNF f = case f of 
                p@(Atom s) -> p 
                p@(Not f) -> p
                a `Or` b -> (s_toDNF a) `Or` (s_toDNF b)
                a `And` b -> let a_dnf = s_toDNF a in case a_dnf of 
                                    (g1 `Or` g2) -> s_toDNF $ (g1 `And` b) `Or` (g2 `And` b)
                                    otherwise ->  let b_dnf = s_toDNF b in case b_dnf of
                                            (c1 `Or` c2) -> s_toDNF $ (c1 `And` a_dnf) `Or` (c2 `And` a_dnf)
                                            otherwise -> a_dnf `And` b_dnf

toDNF = s_toDNF . toNNF
                 