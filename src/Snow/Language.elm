module Snow.Language exposing
    ( BinOp(..)
    , Id
    , Literal(..)
    , SnowExpr(..)
    )


type alias Id =
    String


type SnowExpr
    = SVar Id -- f
    | SLambda Id SnowExpr -- \x -> x
    | SApply SnowExpr SnowExpr -- f x
    | SLiteral Literal -- 5
    | SBinOp BinOp SnowExpr SnowExpr


type BinOp
    = Addition
    | Subtraction
    | Multiplication


type Literal
    = LInt Int
