module Main exposing (main)

import AStar exposing (findPath, straightLineCost)
import Animator exposing (color)
import Array exposing (Array)
import Base64.Encode as Base64Encode
import Browser
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onKeyUp, onVisibilityChange)
import Bytes exposing (Bytes)
import Canvas exposing (Point, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Canvas.Texture exposing (sprite)
import Color
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Image
import Json.Decode as Decode
import Random
import Set
import Task
import Zip exposing (Zip)
import Zip.Entry


type alias Model =
    { documentState : DocumentState
    }


type DocumentState
    = WaitingOnUser
    | Loading
    | Ready String String
    | Failed String


initialModel : Model
initialModel =
    { documentState = WaitingOnUser
    }


type Msg
    = OpenFile
    | FileLoaded File
    | GotZip (Maybe Zip)
    | OpenImage
    | ImageLoaded File
    | ImageBytesLoaded Bytes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenFile ->
            ( model, requestFile )

        OpenImage ->
            ( model, requestImage )

        FileLoaded file ->
            let
                _ =
                    Debug.log "file" file
            in
            ( model, file |> File.toBytes |> Task.map Zip.fromBytes |> Task.perform GotZip )

        GotZip Nothing ->
            ( model, Cmd.none )

        GotZip (Just zip) ->
            let
                -- stuff =
                --     Zip.entries zip
                stuff =
                    Zip.getEntry "test.json" zip

                jpgInZip =
                    Zip.getEntry "thumbs-up.jpg" zip

                _ =
                    Debug.log "stuff" stuff
            in
            case stuff of
                Nothing ->
                    ( model, Cmd.none )

                Just entry ->
                    case Zip.Entry.toString entry of
                        Result.Err err ->
                            ( model, Cmd.none )

                        Result.Ok jsonString ->
                            case jpgInZip of
                                Nothing ->
                                    let
                                        _ =
                                            Debug.log "No jpg found in zip" ""
                                    in
                                    ( model, Cmd.none )

                                Just jpgEntry ->
                                    case Zip.Entry.toBytes jpgEntry of
                                        Result.Err jpgEntryBytesError ->
                                            let
                                                _ =
                                                    Debug.log "failed convert jpg entry to bytes" jpgEntryBytesError
                                            in
                                            ( model, Cmd.none )

                                        Result.Ok jpgBytes ->
                                            let
                                                image =
                                                    Base64Encode.encode (Base64Encode.bytes jpgBytes)

                                                _ =
                                                    Debug.log "image" image
                                            in
                                            ( { model | documentState = Ready jsonString image }, Cmd.none )

        ImageLoaded imageFile ->
            ( model, parseImageFileBytes imageFile )

        ImageBytesLoaded imageBytes ->
            ( { model | documentState = Ready "'{}'" (Base64Encode.encode (Base64Encode.bytes imageBytes)) }, Cmd.none )


requestFile : Cmd Msg
requestFile =
    Select.file [ "application/zip" ] FileLoaded


requestImage : Cmd Msg
requestImage =
    Select.file [ "image/jpeg", "image/png" ] ImageLoaded


parseImageFileBytes : File -> Cmd Msg
parseImageFileBytes file =
    Task.perform ImageBytesLoaded (File.toBytes file)


view : Model -> Html Msg
view model =
    div [ class "w-full" ]
        -- [ Canvas.toHtmlWith
        --     { width = 300
        --     , height = 300
        --     , textures = []
        --     }
        --     [ class "block pixel-art" ]
        --     [ shapes
        --         [ fill (Color.rgb 0.85 0.92 1) ]
        --         [ rect ( 0, 0 ) 300 300 ]
        --     ]
        -- ]
        [ header [ class "antialiased" ]
            [ nav [ class "bg-white border-gray-200 px-4 lg:px-6 py-2.5 dark:bg-gray-800" ]
                [ div [ class "flex flex-wrap justify-between items-center" ]
                    [ div [ class "flex justify-start items-center" ]
                        [ a [ href "https://github.com/JesterXL/final-cow-legend-level-editor", class "flex mr-4" ]
                            [ img [ src "logo.jpg", class "mr-3 h-8" ] []
                            , div [ class "flex flex-row items-baseline" ]
                                [ span [ class "text-2xl font-semibold whitespace-nowrap dark:text-white" ] [ text "Level Editor" ]
                                , span [ class "text-2xl font-semibold text-xs whitespace-nowrap dark:text-white pl-2" ] [ text "v1.0" ]
                                ]
                            ]
                        ]
                    , div [ class "flex items-center lg:order-2" ]
                        [ button [ class "hidden sm:inline-flex items-center justify-center text-white bg-primary-700 hover:bg-primary-800 focus:ring-4 focus:ring-primary-300 font-medium rounded-lg text-xs px-3 py-1.5 mr-2 dark:bg-primary-600 dark:hover:bg-primary-700 focus:outline-none dark:focus:ring-primary-800", onClick OpenImage ] [ text "Open Image" ]
                        , button [ class "hidden sm:inline-flex items-center justify-center text-white bg-primary-700 hover:bg-primary-800 focus:ring-4 focus:ring-primary-300 font-medium rounded-lg text-xs px-3 py-1.5 mr-2 dark:bg-primary-600 dark:hover:bg-primary-700 focus:outline-none dark:focus:ring-primary-800", onClick OpenFile ] [ text "Open File" ]
                        ]
                    ]
                ]
            , div
                [ class "flex flex-row flex-nowrap" ]
                (case model.documentState of
                    Ready jsonString imageString ->
                        [ div [ class "overflow-hidden" ] [ img [ class "basis-6/12", src ("data:image/jpg;base64," ++ imageString) ] [ text "image here" ] ]
                        , div [ class "text-black p-2 basis-2/12" ] [ text jsonString ]
                        ]

                    _ ->
                        [ span [ class "basis-10/12" ] [] ]
                )
            ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Sub.batch
    --     [ onKeyDown keyDecoderPressed
    --     , onKeyUp keyDecoderReleased
    --     ]
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
