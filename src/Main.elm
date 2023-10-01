port module Main exposing (main)

import AStar exposing (findPath, straightLineCost)
import Animator exposing (color)
import Array exposing (Array)
import Base64.Encode as Base64Encode
import Browser
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onKeyUp, onVisibilityChange)
import Bytes exposing (Bytes)
import Bytes.Encode as BytesEncode
import Canvas exposing (Point, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Canvas.Texture exposing (Texture, sprite)
import Color
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Image
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Set
import String exposing (toFloat)
import Task
import Time
import Zip exposing (Zip)
import Zip.Entry


type alias Model =
    { documentState : DocumentState
    , zone : Time.Zone
    , time : Time.Posix
    }


type DocumentState
    = WaitingOnUser
    | Loading
    | GotImageSelected JSONString ImageAsString Bytes
    | Ready
        { jsonString : JSONString
        , imageBytes : Bytes
        , base64Image : ImageAsString
        , sprite : Texture
        , imageOffsetX : Float
        , imageOffsetY : Float
        , tiles : World
        , canvasScale : Float
        }
    | Failed String


type alias JSONString =
    String


type alias ImageAsString =
    String


type alias World =
    Array (Array TileType)


defaultWorld : World
defaultWorld =
    Array.repeat 32 (Array.repeat 35 NotWalkable)



-- Array.repeat 8 (Array.repeat 8 NotWalkable)


type Row
    = Row Int


type Col
    = Col Int


type TileType
    = Walkable
    | NotWalkable


initialModel : Model
initialModel =
    { documentState = WaitingOnUser
    , zone = Time.utc
    , time = Time.millisToPosix 0
    }


type alias ImageName =
    String


type alias ImageBase64 =
    String


type Msg
    = OpenFile
    | FileLoaded File
    | GotZip (Maybe Zip)
    | OpenImage
    | ImageLoaded File
    | ImageBytesLoaded ImageName ImageBase64 Bytes
    | ImageBytesLoadedFailed
    | ImageLoadedFromJavaScript Decode.Value
    | Keyboard KeyPressOrRelease
    | ImageOffsetXChange String
    | ImageOffsetYChange String
    | CanvasScaleChange String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | SaveLevel { jsonString : JSONString, imageBytes : Bytes, imageOffsetX : Float, imageOffsetY : Float, canvasScale : Float }


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
                isFile =
                    \entry -> not (Zip.Entry.isDirectory entry)

                isMapJPG =
                    \entry -> Zip.Entry.basename entry == "map.jpg"

                isMapPNG =
                    \entry -> Zip.Entry.basename entry == "map.png"

                isMapJSON =
                    \entry -> Zip.Entry.basename entry == "map.json"

                -- allDemEntries =
                --     Zip.entries zip
                --         |> List.filter isFile
                --         |> List.map (\entry -> Zip.Entry.basename entry)
                -- _ =
                --     Debug.log "allDemEntries" allDemEntries
                mapJSONMaybe =
                    Zip.entries zip
                        |> List.filter isMapJSON
                        |> List.head

                mapJPGMaybe =
                    Zip.entries zip
                        |> List.filter isMapJPG
                        |> List.head
            in
            case mapJSONMaybe of
                Nothing ->
                    ( model, Cmd.none )

                Just mapJSON ->
                    case mapJPGMaybe of
                        Nothing ->
                            ( model, Cmd.none )

                        Just mapJPG ->
                            case Zip.Entry.toString mapJSON of
                                Result.Err mapJSONErr ->
                                    let
                                        _ =
                                            Debug.log "failed convert mapJSON entry to string" mapJSONErr
                                    in
                                    ( model, Cmd.none )

                                Result.Ok jsonString ->
                                    case Zip.Entry.toBytes mapJPG of
                                        Result.Err mapJPGErr ->
                                            let
                                                _ =
                                                    Debug.log "failed convert jpg entry to bytes" mapJPGErr
                                            in
                                            ( model, Cmd.none )

                                        Result.Ok jpgBytes ->
                                            let
                                                imageBase64 =
                                                    Base64Encode.encode (Base64Encode.bytes jpgBytes)
                                            in
                                            ( { model | documentState = GotImageSelected jsonString imageBase64 jpgBytes }, loadImageURL imageBase64 )

        -- case allImages of
        --     Nothing ->
        --         ( model, Cmd.none )
        --     Just entry ->
        --         case Zip.Entry.toString entry of
        --             Result.Err err ->
        --                 ( model, Cmd.none )
        --             Result.Ok jsonString ->
        --                 case jpgInZip of
        --                     Nothing ->
        --                         let
        --                             _ =
        --                                 Debug.log "No jpg found in zip" ""
        --                         in
        --                         ( model, Cmd.none )
        --                     Just jpgEntry ->
        --                         case Zip.Entry.toBytes jpgEntry of
        --                             Result.Err jpgEntryBytesError ->
        --                                 let
        --                                     _ =
        --                                         Debug.log "failed convert jpg entry to bytes" jpgEntryBytesError
        --                                 in
        --                                 ( model, Cmd.none )
        --                             Result.Ok jpgBytes ->
        --                                 let
        --                                     image =
        --                                         Base64Encode.encode (Base64Encode.bytes jpgBytes)
        --                                     _ =
        --                                         Debug.log "image" image
        --                                 in
        --                                 ( { model | documentState = GotImageSelected jsonString image jpgBytes }, Cmd.none )
        ImageLoaded imageFile ->
            ( model, parseImageFileBytes imageFile )

        ImageBytesLoaded imageName imageBase64 imageBytes ->
            ( { model | documentState = GotImageSelected "'{}'" imageBase64 imageBytes }, loadImageURL imageBase64 )

        ImageBytesLoadedFailed ->
            ( model, Cmd.none )

        ImageLoadedFromJavaScript image ->
            let
                _ =
                    Debug.log "texturefrom dom image" (Canvas.Texture.fromDomImage image)
            in
            case Canvas.Texture.fromDomImage image of
                Nothing ->
                    ( model, Cmd.none )

                Just texture ->
                    let
                        { width, height } =
                            Canvas.Texture.dimensions texture

                        sprite =
                            Canvas.Texture.sprite
                                { x = 0
                                , y = 0
                                , width = width
                                , height = height
                                }
                                texture
                    in
                    case model.documentState of
                        GotImageSelected jsonString imageAsString imageBytes ->
                            ( { model
                                | documentState =
                                    Ready
                                        { jsonString = jsonString
                                        , imageBytes = imageBytes
                                        , base64Image = imageAsString
                                        , sprite = sprite
                                        , imageOffsetX = 0
                                        , imageOffsetY = 0
                                        , tiles = defaultWorld
                                        , canvasScale = 1
                                        }
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

        Keyboard keyPressOrRelease ->
            ( model, Cmd.none )

        ImageOffsetXChange imageOffsetXString ->
            case model.documentState of
                Ready imageState ->
                    case String.toFloat imageOffsetXString of
                        Nothing ->
                            ( model, Cmd.none )

                        Just imageOffsetX ->
                            ( { model | documentState = Ready { imageState | imageOffsetX = imageOffsetX } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ImageOffsetYChange imageOffsetYString ->
            case model.documentState of
                Ready imageState ->
                    case String.toFloat imageOffsetYString of
                        Nothing ->
                            ( model, Cmd.none )

                        Just imageOffsetY ->
                            ( { model | documentState = Ready { imageState | imageOffsetY = imageOffsetY } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CanvasScaleChange scaleString ->
            case model.documentState of
                Ready imageState ->
                    case String.toFloat scaleString of
                        Nothing ->
                            ( model, Cmd.none )

                        Just canvasScale ->
                            ( { model | documentState = Ready { imageState | canvasScale = canvasScale } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        SaveLevel { jsonString, imageBytes, imageOffsetX, imageOffsetY, canvasScale } ->
            let
                encodedJSON =
                    Encode.encode 4 (jsonEncoder imageOffsetX imageOffsetY canvasScale)
                        |> BytesEncode.string
                        |> BytesEncode.encode
                        |> Zip.Entry.store
                            { path = "map.json"
                            , lastModified = ( model.zone, model.time )
                            , comment = Nothing
                            }

                encodedJPG =
                    Zip.Entry.store
                        { path = "map.jpg"
                        , lastModified = ( model.zone, model.time )
                        , comment = Nothing
                        }
                        imageBytes

                newZipBytes =
                    Zip.fromEntries [ encodedJSON, encodedJPG ]
                        |> Zip.toBytes
            in
            ( model, Download.bytes "dat.zip" "application/zip" newZipBytes )


requestFile : Cmd Msg
requestFile =
    Select.file [ "application/zip" ] FileLoaded


requestImage : Cmd Msg
requestImage =
    Select.file [ "image/jpeg", "image/png" ] ImageLoaded


parseImageFileBytes : File -> Cmd Msg
parseImageFileBytes file =
    Task.sequence
        [ File.toUrl file
            |> Task.andThen (\url -> Task.succeed (getFileInfo (Just url) Nothing))
        , File.toBytes file
            |> Task.andThen (\bytes -> Task.succeed (getFileInfo Nothing (Just bytes)))
        ]
        |> Task.perform
            (\taskResult ->
                case taskResult of
                    [ urlFileInfo, bytesFileInfo ] ->
                        case urlFileInfo.url of
                            Nothing ->
                                ImageBytesLoadedFailed

                            Just url ->
                                case bytesFileInfo.bytes of
                                    Nothing ->
                                        ImageBytesLoadedFailed

                                    Just bytes ->
                                        ImageBytesLoaded (File.name file) url bytes

                    _ ->
                        ImageBytesLoadedFailed
            )


type alias FileInfo =
    { url : Maybe String
    , bytes : Maybe Bytes
    }


getFileInfo : Maybe String -> Maybe Bytes -> FileInfo
getFileInfo url bytes =
    { url = url
    , bytes = bytes
    }


keyDecoderPressed : Decode.Decoder Msg
keyDecoderPressed =
    Decode.map toKeyPressed (Decode.field "key" Decode.string)


toKeyPressed : String -> Msg
toKeyPressed string =
    -- let
    --     _ =
    --         Debug.log "key pressed" string
    -- in
    case string of
        "o" ->
            Keyboard OPressed

        _ ->
            Keyboard OtherKeyPressed


keyDecoderReleased : Decode.Decoder Msg
keyDecoderReleased =
    Decode.map toKeyReleased (Decode.field "key" Decode.string)


toKeyReleased : String -> Msg
toKeyReleased string =
    -- let
    --     _ =
    --         Debug.log "key released" string
    -- in
    case string of
        "o" ->
            Keyboard OReleased

        _ ->
            Keyboard OtherKeyReleased


type KeyPressOrRelease
    = OPressed
    | OReleased
    | OtherKeyPressed
    | OtherKeyReleased


jsonEncoder : Float -> Float -> Float -> Encode.Value
jsonEncoder imageOffsetX imageOffsetY canvasScale =
    Encode.object
        [ ( "imageOffsetX", Encode.float imageOffsetX )
        , ( "imageOffsetY", Encode.float imageOffsetY )
        , ( "canvasScale", Encode.float canvasScale )
        ]


port loadImageURL : String -> Cmd msg


port onImageFromJavaScript : (Decode.Value -> msg) -> Sub msg


view : Model -> Html Msg
view model =
    div [ class "w-full" ]
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
            , case model.documentState of
                Ready { jsonString, imageBytes, base64Image, sprite, imageOffsetX, imageOffsetY, tiles, canvasScale } ->
                    let
                        widthAndHeight =
                            Canvas.Texture.dimensions sprite
                    in
                    div
                        [ class "flex flex-row flex-nowrap" ]
                        [ div [ class "grow" ]
                            [ Canvas.toHtmlWith
                                { width = round widthAndHeight.width
                                , height = round widthAndHeight.height
                                , textures = []
                                }
                                [ class "block pixel-art" ]
                                [ shapes
                                    [ fill (Color.rgb 0.85 0.92 1) ]
                                    [ rect ( 0, 0 ) widthAndHeight.width widthAndHeight.height ]
                                , Canvas.group
                                    [ Canvas.Settings.Advanced.transform [ Canvas.Settings.Advanced.scale canvasScale canvasScale ] ]
                                    ([ Canvas.texture
                                        [ Canvas.Settings.Advanced.imageSmoothing False ]
                                        ( 0, 0 )
                                        sprite
                                     ]
                                        ++ drawWorld tiles imageOffsetX imageOffsetY
                                    )
                                ]
                            ]
                        , div [ class "w-[360px] h-screen p-4 overflow-y-auto bg-white dark:bg-gray-800" ]
                            [ h5 [ class "inline-flex items-center mb-6 text-sm font-semibold text-gray-500 uppercase dark:text-gray-400" ] [ text "Properties" ]
                            , Html.form [ class "text-white" ]
                                [ div [ class "space-y-4" ]
                                    [ div []
                                        [ label [ class "block mb-2 text-sm font-medium text-gray-900 dark:text-white" ] [ text "Offset X:" ]
                                        , input
                                            [ class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                            , onInput ImageOffsetXChange
                                            , value (String.fromFloat imageOffsetX)
                                            ]
                                            []
                                        ]
                                    , div []
                                        [ label [ class "block mb-2 text-sm font-medium text-gray-900 dark:text-white" ] [ text "Offset Y:" ]
                                        , input
                                            [ class "bbg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                            , onInput ImageOffsetYChange
                                            , value (String.fromFloat imageOffsetY)
                                            ]
                                            []
                                        ]
                                    , div []
                                        [ label [ class "block mb-2 text-sm font-medium text-gray-900 dark:text-white" ] [ text "Scale:" ]
                                        , input
                                            [ class "bbg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                            , onInput CanvasScaleChange
                                            , value (String.fromFloat canvasScale)
                                            ]
                                            []
                                        , input
                                            [ class "mt-4 w-full h-2 bg-gray-200 rounded-lg appearance-none cursor-pointer dark:bg-gray-700"
                                            , Html.Attributes.type_ "range"
                                            , onInput CanvasScaleChange
                                            , Html.Attributes.min "1"
                                            , Html.Attributes.max "6"
                                            , Html.Attributes.step "0.1"
                                            , value (String.fromFloat canvasScale)
                                            ]
                                            []

                                        -- class="w-full h-2 bg-gray-200 rounded-lg appearance-none cursor-pointer dark:bg-gray-700">
                                        ]
                                    , div []
                                        [ label [ class "block mb-2 text-sm font-medium text-gray-900 dark:text-white" ] [ text "Map JSON:" ]
                                        , textarea
                                            [ rows 4, class "block p-2.5 w-full text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500" ]
                                            [ text jsonString ]
                                        ]
                                    , button [ onClick (SaveLevel { jsonString = jsonString, imageBytes = imageBytes, imageOffsetX = imageOffsetX, imageOffsetY = imageOffsetY, canvasScale = canvasScale }) ] [ text "Save" ]
                                    ]
                                ]
                            ]
                        ]

                _ ->
                    div [] []
            ]
        ]


drawWorld : World -> Float -> Float -> List Canvas.Renderable
drawWorld world imageOffsetX imageOffsetY =
    [ Canvas.group
        [ Canvas.Settings.Advanced.alpha 0.5
        , Canvas.Settings.Advanced.transform
            [ Canvas.Settings.Advanced.translate imageOffsetX imageOffsetY
            ]
        ]
        (Array.indexedMap
            (\rowIndex row ->
                Array.indexedMap
                    (\colIndex cell ->
                        drawCell (Row rowIndex) (Col colIndex) cell
                    )
                    row
                    |> Array.toList
                    |> List.concatMap
                        (\cell -> cell)
            )
            world
            |> Array.toList
            |> List.concatMap
                (\cell -> cell)
        )
    ]


drawCell : Row -> Col -> TileType -> List Canvas.Renderable
drawCell (Row row) (Col col) tileType =
    [ shapes
        [ if tileType == Walkable then
            fill Color.green

          else
            fill Color.red
        ]
        [ rect ( Basics.toFloat col * 16, Basics.toFloat row * 16 ) 16 16 ]
    , shapes [ stroke Color.lightGreen ] [ rect ( Basics.toFloat col * 16, Basics.toFloat row * 16 ) 16 16 ]
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onImageFromJavaScript ImageLoadedFromJavaScript
        , onKeyDown keyDecoderPressed
        , onKeyUp keyDecoderReleased
        , Time.every (10 * 1000) Tick
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
