module Snow.Parser exposing (..)

import Parser exposing ((|.), (|=), Parser, symbol)
import Parser.Expression exposing (Assoc(..), Operator(..))
import Pratt exposing (Config)
import Snow.Language exposing (BinOp(..), Literal(..), SnowExpr(..))


type Error
    = Error


parse : String -> Result Error SnowExpr
parse =
    Parser.run parseExpr
        >> Result.mapError
            (\err ->
                let
                    _ =
                        Debug.log "parse error" err
                in
                Error
            )


parseSnow : Parser SnowExpr
parseSnow =
    Parser.succeed identity
        |= parseExpr
        |. Parser.end


parseExpr : Parser SnowExpr
parseExpr =
    Pratt.expression
        { oneOf =
            [ parseInt
            , parseLambda
            , parseVariable
            , parenthesizedExpression
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 10 (Parser.symbol "+") (SBinOp Addition)
            , Pratt.infixLeft 10 (Parser.symbol "-") (SBinOp Subtraction)
            , Pratt.infixLeft 20 (Parser.symbol "*") (SBinOp Multiplication)

            -- , Pratt.infixLeft 20 (Parser.symbol "/") (/)
            -- , Pratt.infixRight 30 (Parser.symbol "^") (^)
            , paseApply
            ]
        , spaces = Parser.spaces
        }


parenthesizedExpression : Config SnowExpr -> Parser SnowExpr
parenthesizedExpression config =
    Parser.succeed identity
        |. symbol "("
        |= Pratt.subExpression 0 config
        |. symbol ")"


paseApply : Config SnowExpr -> ( Int, SnowExpr -> Parser SnowExpr )
paseApply config =
    ( 50
    , \left ->
        Parser.succeed (SApply left)
            |. Parser.spaces
            |= Pratt.subExpression 50 config
    )


negateExpr : SnowExpr -> SnowExpr
negateExpr expr =
    case expr of
        SLiteral lit ->
            case lit of
                LInt i ->
                    negate i
                        |> LInt
                        |> SLiteral

        _ ->
            expr


parseLambda : Config SnowExpr -> Parser SnowExpr
parseLambda config =
    Parser.succeed SLambda
        |. Parser.spaces
        |. Parser.symbol "\\"
        |. Parser.spaces
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompIf (\c -> Char.isAlpha c && Char.isLower c)
                    |. Parser.chompWhile Char.isAlphaNum
           )
        |. Parser.spaces
        |. Parser.symbol "->"
        |. Parser.spaces
        |= Pratt.subExpression 0 config


parseVariable : Config SnowExpr -> Parser SnowExpr
parseVariable _ =
    Parser.succeed SVar
        |. Parser.spaces
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompIf (\c -> Char.isAlpha c && Char.isLower c)
                    |. Parser.chompWhile Char.isAlphaNum
           )


parseInt : Config SnowExpr -> Parser SnowExpr
parseInt =
    Parser.succeed (\sign value -> LInt (sign value))
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.symbol "-" |> Parser.map (always negate)
            , Parser.symbol "+" |> Parser.map (always identity)
            , Parser.succeed identity
            ]
        |= Parser.int
        |. Parser.spaces
        |> Parser.backtrackable
        |> Parser.map SLiteral
        |> Pratt.literal
