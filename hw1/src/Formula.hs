module Formula where

data Formula = Atom { _name :: String } 
			 | Not Formula  
			 | Formula `Or` Formula   
			 | Formula `And` Formula 
			 | Formula :-> Formula 
			 | Formula :<->: Formula

infixl 7 `And`
infixl 7 :->
infixl 7 :<->:
infixl 6 `Or`

instance Show Formula where
	showsPrec p f = case f of 
		(Atom s) -> showString s
	  	(Not f) -> showParen True $ showString "!" . showsPrec 10 f

		(x `Or` y) -> formula_show_helper " || " x y p 6
	  	(x `And` y) -> formula_show_helper " && " x y p 7
	  	(x :-> y) -> formula_show_helper " -> " x y p 7
	  	(x :<->: y) -> formula_show_helper " <-> " x y p 7

		where formula_show_helper str x y p lim = showParen (p > lim) $ showsPrec l x . showString str . showsPrec l y
												where l = succ lim

p1 = (Atom "p1")
p2 = (Atom "p2")
p3 = (Atom "p3")

sampleOr = (Atom "p1") `Or` (Atom "p2")
sample1 = Not (p1 `Or` p2)
sample3 = Not (p1 `And` p2) `And` p3
