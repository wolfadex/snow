port module Main exposing (main)

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Platform
import Snow.Compiler.JavaScript
import Snow.Language exposing (SnowExpr)
import Snow.Parser exposing (Error(..))


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , subscriptions = subscriptions
        , update = update
        }


type alias Model =
    { placeholder : String
    , snowResult : Result Error SnowExpr
    }


type alias Flags =
    List String


init : Flags -> ( Model, Cmd Msg )
init args =
    let
        _ =
            Debug.log "args" args

        plusTwo =
            (+) 2
    in
    ( { placeholder = plusTwo 3 |> String.fromInt
      , snowResult = Err Error
      }
    , case args of
        [] ->
            "Expected a path to an Snow file but found nothing"
                |> Json.Encode.string
                |> logError

        fileToLoad :: _ ->
            fileToLoad
                |> Json.Encode.string
                |> loadFile
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    fileLoaded FileLoaded


port fileLoaded : (Value -> msg) -> Sub msg


type Msg
    = NoOp
    | FileLoaded Value


port log : Value -> Cmd msg


port logError : Value -> Cmd msg


port loadFile : Value -> Cmd msg


port compilerOutput : Value -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , "No Op"
                |> Json.Encode.string
                |> log
            )

        FileLoaded val ->
            case Json.Decode.decodeValue decodeFileLoaded val of
                Err err ->
                    ( model
                    , err
                        |> Json.Decode.errorToString
                        |> Json.Encode.string
                        |> logError
                    )

                Ok ( _, fileContent ) ->
                    let
                        _ =
                            Debug.log "fileContent" fileContent

                        snowResult =
                            Snow.Parser.parse fileContent
                    in
                    ( { model
                        | snowResult = snowResult
                      }
                    , case snowResult of
                        Err err ->
                            err
                                |> Debug.toString
                                |> Json.Encode.string
                                |> logError

                        Ok expr ->
                            [ ( "filePath", Json.Encode.string "dist/js/snow.js" )
                            , ( "content"
                              , expr
                                    |> Snow.Compiler.JavaScript.compile
                                    |> Json.Encode.string
                              )
                            ]
                                |> Json.Encode.object
                                |> compilerOutput
                    )


decodeFileLoaded : Decoder ( String, String )
decodeFileLoaded =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "content" Json.Decode.string)
