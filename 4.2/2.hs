data Expression = Const Integer
    | X
    | Plus Expression Expression
    | Minus Expression Expression
    | Multiply Expression Expression
    | Divide Expression Expression
    | Power Expression Integer
    | LeftMinus Expression 

instance Show Expression where
    show (Const c) = show c
    show X = "x" 
    show (Plus a b) = show a ++ " + " ++ show b
    show (Minus a b) = show a ++ " - (" ++ show b ++ ")"
    show (Multiply a b) = show a ++ " * (" ++ show b ++ ")"
    show (Divide a b) = show a ++ " / (" ++ show b ++ ")"
    show (Power a b) = "(" ++ show a ++ ") ^ (" ++ show b ++ ")"
    show (LeftMinus a) = " - (" ++ show a ++ ")"

simplify :: Expression -> Expression
simplify (Const c) = Const c
simplify (X) = X
simplify (Plus a b) = simplifyHelper (Plus (simplify a) (simplify b))
simplify (Minus a b) = simplifyHelper (Minus (simplify a) (simplify b))
simplify (Multiply a b) = simplifyHelper (Multiply (simplify a) (simplify b))
simplify (Divide a b) = simplifyHelper (Divide (simplify a) (simplify b))
simplify (Power a b) = simplifyHelper (Power (simplify a) b)
simplify (LeftMinus a) = simplifyHelper (LeftMinus (simplify a))

simplifyHelper :: Expression -> Expression
simplifyHelper (Const c) = Const c

simplifyHelper (X) = X

simplifyHelper (Plus (Const 0) b) = b
simplifyHelper (Plus a (Const 0)) = a
simplifyHelper (Plus (Const c1) (Const c2)) = Const (c1+c2)
simplifyHelper (Plus X X) = Multiply (Const 2) X
simplifyHelper (Plus (Multiply (Const c1) X) (Multiply (Const c2) X)) = Multiply (Const (c1+c2)) X
simplifyHelper (Plus a b) = Plus a b

simplifyHelper (Minus (Const 0) b) = simplifyHelper (LeftMinus(b)) 
simplifyHelper (Minus a (Const 0)) = a
simplifyHelper (Minus (Const c1) (Const c2)) = Const (c1-c2)
simplifyHelper (Minus (Multiply (Const c1) X) (Multiply (Const c2) X)) = Multiply (Const (c1-c2)) X


simplifyHelper (Multiply (Const 0) _) = Const 0 
simplifyHelper (Multiply _ (Const 0)) = Const 0 
simplifyHelper (Multiply (Const 1) b) = b 
simplifyHelper (Multiply a (Const 1)) = a
simplifyHelper (Multiply (Const 1) b) = b 
simplifyHelper (Multiply a (Const 1)) = a 
simplifyHelper (Multiply a (Const (-1))) = simplifyHelper (LeftMinus(a)) 
simplifyHelper (Multiply (Const (-1)) b) = simplifyHelper (LeftMinus(b))
simplifyHelper (Multiply (Const c1) (Const c2)) = Const (c1 * c2)
simplifyHelper (Multiply X X) = Power X 2
simplifyHelper (Multiply (Power X a) X) = Power X (a + 1)
simplifyHelper (Multiply X (Power X b)) = Power X (b + 1)
simplifyHelper (Multiply (Power X a) (Power X b)) = Power X (a + b)
simplifyHelper (Multiply a b) = Multiply a b

simplifyHelper (Divide (Const 0) b) = Const 0
simplifyHelper (Divide (Const c1) (Const c2)) = Const (c1 `div` c2)
simplifyHelper (Divide a (Const 1)) = a
simplifyHelper (Divide X X) = Const 1
simplifyHelper (Divide (Power X a) X) = simplifyHelper (Power X (a - 1))
simplifyHelper (Divide X (Power X b)) = simplifyHelper (Power X (1 - b))
simplifyHelper (Divide (Power X a) (Power X b)) = simplifyHelper (Power X (a - b))
simplifyHelper (Divide a b) = Divide a b

simplifyHelper (Power a 0) = Const 1
simplifyHelper (Power a 1) = a
simplifyHelper (Power a x) = Power a x

simplifyHelper (LeftMinus(Const c1)) = Const (-c1)
simplifyHelper (LeftMinus a) = LeftMinus a



derivative :: Expression -> Expression
derivative expr = simplify (derivativeHelper expr)

derivativeHelper :: Expression -> Expression
derivativeHelper (Const _) = Const 0
derivativeHelper (X) = Const 1
derivativeHelper (LeftMinus a) = LeftMinus (derivativeHelper a)
derivativeHelper (Plus a b) = simplifyHelper $ Plus (derivativeHelper a) (derivativeHelper b)
derivativeHelper (Minus a b) = simplifyHelper $ Minus (derivativeHelper a) (derivativeHelper b)
derivativeHelper (Multiply a b) = simplifyHelper $ Plus (simplifyHelper $ Multiply (derivativeHelper a) b) (simplifyHelper $ Multiply (derivativeHelper b) a)
derivativeHelper (Divide a b) = simplifyHelper $ Divide(simplifyHelper (Minus (Multiply (derivativeHelper a) b) (Multiply (derivativeHelper b) a))) (Power b 2)
derivativeHelper (Power a b) = simplifyHelper $ Multiply (Multiply (Const b) (Power a (b - 1))) (derivativeHelper a)


--example input: derivative (Plus X X)
--example input: derivative (Divide X X)
--example input: derivative (Divide X X)
--example input: derivative (Plus (Multiply (X) (Const 5)) (Power (Plus (Multiply (X) (Const 0))(Multiply X X))2))



