module SF where
import Formula

-- rewrite formula without (->) and (<->)
simplify :: Formula -> Formula
simplify (Atom s) = Atom s
simplify (Not f) = Not (simplify f)
simplify (a `Or` b) = (simplify a) `Or` (simplify b)
simplify (a `And` b) = (simplify a) `And` (simplify b)
simplify (a :-> b) = simplify $ (Not a) `Or` b
simplify (a :<->: b) = simplify $ (a :-> b) `And` (b :-> a)
