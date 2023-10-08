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
import Html.Events exposing (on, onClick, onInput)
import Image
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Set
import String exposing (toFloat)
import Task
import Time
import Vector30
import Vector31
import Zip exposing (Zip)
import Zip.Entry


type alias Model =
    { documentState : DocumentState
    , zone : Time.Zone
    , time : Time.Posix
    , canvasBoundingRect : CanvasBoundingRect
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
    Vector30.Vector30 (Vector31.Vector31 TileType)


defaultWorld : World
defaultWorld =
    Vector31.initializeFromInt (\_ -> NotWalkable)
        |> Vector30.repeat


type Row
    = Row Vector30.Index


type Col
    = Col Vector31.Index


getCell : Row -> Col -> World -> TileType
getCell (Row row) (Col col) world =
    let
        rowVector =
            Vector30.get row world

        tile =
            Vector31.get col rowVector
    in
    tile


setCell : Row -> Col -> TileType -> World -> World
setCell (Row row) (Col col) newValue world =
    let
        rowVector =
            Vector30.get row world

        updatedColVector =
            Vector31.set col newValue rowVector

        updatedRowVector =
            Vector30.set row updatedColVector world
    in
    updatedRowVector


type TileType
    = Walkable
    | NotWalkable


getOppositeTileType : TileType -> TileType
getOppositeTileType tile =
    case tile of
        Walkable ->
            NotWalkable

        NotWalkable ->
            Walkable


initialModel : Model
initialModel =
    { documentState = WaitingOnUser
    , zone = Time.utc
    , time = Time.millisToPosix 0
    , canvasBoundingRect = { x = 0, y = 0, width = 0, height = 0 }
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
    | SaveLevel
        { jsonString : JSONString
        , imageBytes : Bytes
        , imageOffsetX : Float
        , imageOffsetY : Float
        , canvasScale : Float
        , tiles : World
        }
    | MouseClick MouseClickData
    | CanvasBoundingRectLoaded Decode.Value


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

                -- mapJPGMaybe =
                --     Zip.entries zip
                --         |> List.filter isMapJPG
                --         |> List.head
                mapPNGMaybe =
                    Zip.entries zip
                        |> List.filter isMapPNG
                        |> List.head
            in
            case mapJSONMaybe of
                Nothing ->
                    ( model, Cmd.none )

                Just mapJSON ->
                    case mapPNGMaybe of
                        Nothing ->
                            ( model, Cmd.none )

                        Just mapPNG ->
                            case Zip.Entry.toString mapJSON of
                                Result.Err mapJSONErr ->
                                    let
                                        _ =
                                            Debug.log "failed convert mapJSON entry to string" mapJSONErr
                                    in
                                    ( model, Cmd.none )

                                Result.Ok jsonString ->
                                    case Zip.Entry.toBytes mapPNG of
                                        Result.Err mapPNGErr ->
                                            let
                                                _ =
                                                    Debug.log "failed convert png entry to bytes" mapPNGErr
                                            in
                                            ( model, Cmd.none )

                                        Result.Ok pngBytes ->
                                            let
                                                imageBase64 =
                                                    Base64Encode.encode (Base64Encode.bytes pngBytes)
                                            in
                                            ( { model | documentState = GotImageSelected jsonString imageBase64 pngBytes }, loadImageURL imageBase64 )

        ImageLoaded imageFile ->
            ( model, parseImageFileBytes imageFile )

        ImageBytesLoaded imageName imageBase64 imageBytes ->
            ( { model | documentState = GotImageSelected "{}" imageBase64 imageBytes }, loadImageURL imageBase64 )

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
                            let
                                jsonDoc =
                                    case Decode.decodeString jsonDecoder jsonString of
                                        Result.Err err ->
                                            let
                                                _ =
                                                    Debug.log "failed to decode json" err
                                            in
                                            { imageOffsetX = 0.0
                                            , imageOffsetY = 0.0
                                            , canvasScale = 1.0
                                            }

                                        Result.Ok parsedJSONDoc ->
                                            let
                                                _ =
                                                    Debug.log "parsed json successfully" parsedJSONDoc
                                            in
                                            parsedJSONDoc
                            in
                            ( { model
                                | documentState =
                                    Ready
                                        { jsonString = jsonString
                                        , imageBytes = imageBytes
                                        , base64Image = imageAsString
                                        , sprite = sprite
                                        , imageOffsetX = jsonDoc.imageOffsetX
                                        , imageOffsetY = jsonDoc.imageOffsetY
                                        , tiles = defaultWorld
                                        , canvasScale = jsonDoc.canvasScale
                                        }
                              }
                            , getCanvasBoundingRect ()
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
                            ( { model | documentState = Ready { imageState | imageOffsetX = imageOffsetX } }, getCanvasBoundingRect () )

                _ ->
                    ( model, Cmd.none )

        ImageOffsetYChange imageOffsetYString ->
            case model.documentState of
                Ready imageState ->
                    case String.toFloat imageOffsetYString of
                        Nothing ->
                            ( model, Cmd.none )

                        Just imageOffsetY ->
                            ( { model | documentState = Ready { imageState | imageOffsetY = imageOffsetY } }, getCanvasBoundingRect () )

                _ ->
                    ( model, Cmd.none )

        CanvasScaleChange scaleString ->
            case model.documentState of
                Ready imageState ->
                    case String.toFloat scaleString of
                        Nothing ->
                            ( model, Cmd.none )

                        Just canvasScale ->
                            ( { model | documentState = Ready { imageState | canvasScale = canvasScale } }, getCanvasBoundingRect () )

                _ ->
                    ( model, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        SaveLevel { jsonString, imageBytes, imageOffsetX, imageOffsetY, canvasScale, tiles } ->
            let
                encodedTiles =
                    encodeWorld tiles

                encodedJSON =
                    Encode.encode 4 (jsonEncoder imageOffsetX imageOffsetY canvasScale encodedTiles)
                        |> BytesEncode.string
                        |> BytesEncode.encode
                        |> Zip.Entry.store
                            { path = "map.json"
                            , lastModified = ( model.zone, model.time )
                            , comment = Nothing
                            }

                encodedPNG =
                    Zip.Entry.store
                        { path = "map.png"
                        , lastModified = ( model.zone, model.time )
                        , comment = Nothing
                        }
                        imageBytes

                newZipBytes =
                    Zip.fromEntries [ encodedJSON, encodedPNG ]
                        |> Zip.toBytes
            in
            ( model, Download.bytes "dat.zip" "application/zip" newZipBytes )

        MouseClick mouseClickData ->
            case model.documentState of
                Ready imageState ->
                    let
                        x =
                            mouseClickData.x - model.canvasBoundingRect.x - imageState.imageOffsetX - 8

                        y =
                            mouseClickData.y - model.canvasBoundingRect.y - imageState.imageOffsetY - 8 * imageState.canvasScale

                        -- _ =
                        --     Debug.log "mouseClickData" mouseClickData
                        -- _ =
                        --     Debug.log "model.canvasBoundingRect" model.canvasBoundingRect
                        -- _ =
                        --     Debug.log "image offset" ( imageState.imageOffsetX, imageState.imageOffsetY )
                        -- _ =
                        --     Debug.log "x and y" ( x, y )
                        rowIndexClick =
                            round (y / 16)

                        colIndexClick =
                            round (x / 16)

                        maybeRowAndColIndex =
                            Vector30.intToIndex rowIndexClick
                                |> Maybe.andThen
                                    (\rowIndexValue ->
                                        Vector31.intToIndex colIndexClick
                                            |> Maybe.map (\colIndexValue -> ( rowIndexValue, colIndexValue ))
                                    )
                    in
                    -- case cellMaybe of
                    --     Nothing ->
                    --         ( model, Cmd.none )
                    --     Just tileWalkableState ->
                    --         let
                    --             updatedTiles =
                    --                 Array.set colIndex (getOppositeTileType tileWalkableState) row)
                    --         in
                    --         ( { model | documentState = {} }, Cmd.none )
                    case maybeRowAndColIndex of
                        Just ( rowIndex, colIndex ) ->
                            let
                                tileType =
                                    getCell (Row rowIndex) (Col colIndex) imageState.tiles

                                updatedTileType =
                                    getOppositeTileType tileType

                                updatedWorld =
                                    setCell (Row rowIndex) (Col colIndex) updatedTileType imageState.tiles
                            in
                            ( { model | documentState = Ready { imageState | tiles = updatedWorld } }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CanvasBoundingRectLoaded boundingRectValue ->
            let
                boundingRect =
                    getCanvasBoundingRectElseDefault boundingRectValue
            in
            ( { model
                | canvasBoundingRect =
                    { x = boundingRect.x
                    , y = boundingRect.y
                    , width = boundingRect.width
                    , height = boundingRect.height
                    }
              }
            , Cmd.none
            )


requestFile : Cmd Msg
requestFile =
    Select.file [ "application/zip" ] FileLoaded


requestImage : Cmd Msg
requestImage =
    Select.file [ "image/png" ] ImageLoaded


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


jsonEncoder : Float -> Float -> Float -> Encode.Value -> Encode.Value
jsonEncoder imageOffsetX imageOffsetY canvasScale tilesEncoded =
    Encode.object
        [ ( "imageOffsetX", Encode.float imageOffsetX )
        , ( "imageOffsetY", Encode.float imageOffsetY )
        , ( "canvasScale", Encode.float canvasScale )
        , ( "tiles", tilesEncoded )
        ]


jsonDecoder : Decode.Decoder JSONDecodedDocument
jsonDecoder =
    Decode.map3 JSONDecodedDocument
        (Decode.field "imageOffsetX" Decode.float)
        (Decode.field "imageOffsetY" Decode.float)
        (Decode.field "canvasScale" Decode.float)


type alias JSONDecodedDocument =
    { imageOffsetX : Float
    , imageOffsetY : Float
    , canvasScale : Float
    }


decodeBoundingRect : Decode.Decoder CanvasBoundingRect
decodeBoundingRect =
    Decode.map4 CanvasBoundingRect
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)


getCanvasBoundingRectElseDefault : Encode.Value -> CanvasBoundingRect
getCanvasBoundingRectElseDefault boundingRectValue =
    Decode.decodeValue
        decodeBoundingRect
        boundingRectValue
        |> Result.withDefault
            { x = 0
            , y = 0
            , width = 0
            , height = 0
            }


encodeWorld : World -> Encode.Value
encodeWorld world =
    Encode.list (Encode.list Encode.string) (worldToList world)


tileTypeToString : TileType -> String
tileTypeToString tileType =
    case tileType of
        Walkable ->
            "Walkable"

        NotWalkable ->
            "NotWalkable"


rowTilesToRowStrings : Vector31.Vector31 TileType -> Vector31.Vector31 String
rowTilesToRowStrings rowTiles =
    Vector31.map tileTypeToString rowTiles


rowStringsToListStrings : Vector31.Vector31 String -> List String
rowStringsToListStrings rowStrings =
    Vector31.toList rowStrings


worldToList : Vector30.Vector30 (Vector31.Vector31 TileType) -> List (List String)
worldToList world =
    Vector30.map rowTilesToRowStrings world
        |> Vector30.map rowStringsToListStrings
        |> Vector30.toList


port loadImageURL : String -> Cmd msg


port onImageFromJavaScript : (Decode.Value -> msg) -> Sub msg


port getCanvasBoundingRect : () -> Cmd msg


port onCanvasBoundingRect : (Decode.Value -> msg) -> Sub msg


type alias CanvasBoundingRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


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
                                [ class "block pixel-art", on "click" (Decode.map MouseClick mouseClickDecoder) ]
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
                            , div [ class "text-white" ]
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
                                    , button [ onClick (SaveLevel { jsonString = jsonString, imageBytes = imageBytes, imageOffsetX = imageOffsetX, imageOffsetY = imageOffsetY, canvasScale = canvasScale, tiles = tiles }) ] [ text "Save" ]
                                    ]
                                ]
                            ]
                        ]

                _ ->
                    div
                        [ class "flex flex-row flex-nowrap" ]
                        [ div [ class "grow" ]
                            [ Canvas.toHtmlWith
                                { width = round 600
                                , height = round 600
                                , textures = []
                                }
                                [ class "block pixel-art" ]
                                []
                            ]
                        ]
            ]
        ]


mouseClickDecoder : Decode.Decoder MouseClickData
mouseClickDecoder =
    Decode.map2 MouseClickData
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


type alias MouseClickData =
    { x : Float, y : Float }


drawWorld : World -> Float -> Float -> List Canvas.Renderable
drawWorld world imageOffsetX imageOffsetY =
    [ Canvas.group
        [ Canvas.Settings.Advanced.alpha 0.5
        , Canvas.Settings.Advanced.transform
            [ Canvas.Settings.Advanced.translate imageOffsetX imageOffsetY
            ]
        ]
        (Vector30.indexedMap
            (\rowIndex row ->
                Vector31.indexedMap
                    (\colIndex cell ->
                        drawCell (Row rowIndex) (Col colIndex) cell
                    )
                    row
                    |> Vector31.toList
                    |> List.concatMap
                        (\cell -> cell)
            )
            world
            |> Vector30.toList
            |> List.concatMap
                (\cell -> cell)
        )
    ]


drawCell : Row -> Col -> TileType -> List Canvas.Renderable
drawCell (Row row) (Col col) tileType =
    let
        rowInt =
            Vector30.indexToInt row

        colInt =
            Vector31.indexToInt col
    in
    [ shapes
        [ if tileType == Walkable then
            fill Color.green

          else
            fill Color.red
        ]
        [ rect ( Basics.toFloat colInt * 16, Basics.toFloat rowInt * 16 ) 16 16 ]
    , shapes [ stroke Color.lightGreen ] [ rect ( Basics.toFloat colInt * 16, Basics.toFloat rowInt * 16 ) 16 16 ]
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
        , onCanvasBoundingRect CanvasBoundingRectLoaded
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
