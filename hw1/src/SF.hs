module SF where
import Formula

-- rewrite formula without (->) and (<->)
simplify :: Formula -> Formula
simplify f = case f of 
            Atom s -> Atom s 
            Not f -> Not (simplify f)
            (a `Or` b) -> (simplify a) `Or` (simplify b)
            (a `And` b) -> (simplify a) `And` (simplify b)
            (a :-> b) -> simplify $ (Not a) `Or` b
            (a :<->: b) -> simplify $ (a :-> b) `And` (b :-> a)


