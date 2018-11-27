module Syntax where

type Name = String

data Expr
  = Float Double
  | BinOp Op Expr Expr -- BinOp Plus (Float 2.5) (Float 2.5)
  | Var String -- Var "x"
  | Call Name [Expr] -- any function call
  | Function Name [Expr] Expr -- Function "foo" [all the input args] return type
  | Extern Name [Expr] -- standard library calls
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)
