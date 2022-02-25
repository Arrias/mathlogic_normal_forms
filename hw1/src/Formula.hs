module Formula where

data Formula = Atom { _name :: String } 
			 | Not Formula  
			 | Formula `Or` Formula   
			 | Formula `And` Formula 
			 | Formula :-> Formula 
			 | Formula :<->: Formula
			 	deriving Eq

infixr 7 `And`
infixr 7 :->
infixr 7 :<->:
infixr 6 `Or`

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


