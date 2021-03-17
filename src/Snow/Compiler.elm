module Snow.Compiler exposing (compileToJS)

import Snow.Language exposing (BinOp(..), Literal(..), SnowExpr(..))


compileToJS : SnowExpr -> String
compileToJS expr =
    """const std = @import("std");
const print = std.debug.print;

pub fn main() anyerror!void {
    const result = """ ++ toString expr ++ """;
    print("{}\\n", .{result});
}"""


toString : SnowExpr -> String
toString expr =
    case expr of
        SVar id ->
            id

        SLambda id body ->
            "fn anonymous(" ++ id ++ """: i32): i32 {
    return """ ++ toString body ++ """;
}"""

        SApply left right ->
            toString left ++ " " ++ toString right

        SLiteral (LInt i) ->
            String.fromInt i

        SBinOp op left right ->
            toString left ++ " " ++ compileBinOp op ++ " " ++ toString right


compileBinOp : BinOp -> String
compileBinOp op =
    case op of
        Addition ->
            "+"

        Subtraction ->
            "-"

        Multiplication ->
            "*"
