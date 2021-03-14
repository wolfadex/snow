module Snow.Parser exposing (..)

import Parser exposing ((|.), (|=), Parser, symbol)
import Parser.Expression exposing (Assoc(..), Operator(..))
import Pratt exposing (Config)


type Error
    = Error


type alias Id =
    String


type SnowExpr
    = SVar Id -- f
    | SLambda Id SnowExpr -- \x -> x
    | SApply SnowExpr SnowExpr -- f x
    | SLiteral Literal -- 5


type Literal
    = LInt Int


parse : String -> Result Error SnowExpr
parse =
    Parser.run parseExpr
        >> Result.mapError (\_ -> Error)


parseExpr : Parser SnowExpr
parseExpr =
    Pratt.expression
        { oneOf =
            [ Pratt.prefix 10 (Parser.symbol "-") negateExpr
            , parseLambda
            , parseInt
            , parseVariable
            , parenthesizedExpression
            ]
        , andThenOneOf =
            [ paseApply ]
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
    Parser.succeed LInt
        |= Parser.int
        |> Parser.map SLiteral
        |> Pratt.literal
