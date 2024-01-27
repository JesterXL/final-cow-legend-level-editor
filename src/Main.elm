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
import Canvas.Settings as CanvasSettings
import Canvas.Settings.Advanced
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Canvas.Texture exposing (Texture, sprite)
import Color
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Image
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Set
import String exposing (toFloat)
import Svg
import Svg.Attributes as SvgAttr exposing (..)
import Task
import Time
import Vector29
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
        , imageOffsetXUser : String
        , imageOffsetY : Float
        , imageOffsetYUser : String
        , tiles : World
        , canvasScale : Float
        , canvasScaleUser : String
        }
    | Failed String


type alias JSONString =
    String


type alias ImageAsString =
    String


type alias World =
    Vector29.Vector29 (Vector31.Vector31 TileType)


defaultWorld : World
defaultWorld =
    Vector31.initializeFromInt (\_ -> NotWalkable)
        |> Vector29.repeat


type Row
    = Row Vector29.Index


type Col
    = Col Vector31.Index


getCell : Row -> Col -> World -> TileType
getCell (Row row) (Col col) world =
    let
        rowVector =
            Vector29.get row world

        tile =
            Vector31.get col rowVector
    in
    tile


setCell : Row -> Col -> TileType -> World -> World
setCell (Row row) (Col col) newValue world =
    let
        rowVector =
            Vector29.get row world

        updatedColVector =
            Vector31.set col newValue rowVector

        updatedRowVector =
            Vector29.set row updatedColVector world
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
                isMapPNG =
                    \entry -> Zip.Entry.basename entry == "map.png"

                isMapJSON =
                    \entry -> Zip.Entry.basename entry == "map.json"

                mapJSONMaybe =
                    Zip.entries zip
                        |> List.filter isMapJSON
                        |> List.head

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
                                            , tiles = [ [] ]
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
                                        , imageOffsetXUser = jsonDoc.imageOffsetX |> String.fromFloat
                                        , imageOffsetY = jsonDoc.imageOffsetY
                                        , imageOffsetYUser = jsonDoc.imageOffsetY |> String.fromFloat
                                        , tiles = listToWorld jsonDoc.tiles
                                        , canvasScale = jsonDoc.canvasScale
                                        , canvasScaleUser = jsonDoc.canvasScale |> String.fromFloat
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
                            ( { model
                                | documentState =
                                    Ready
                                        { imageState
                                            | imageOffsetXUser = imageOffsetXString
                                        }
                              }
                            , Cmd.none
                            )

                        Just imageOffsetX ->
                            ( { model
                                | documentState =
                                    Ready
                                        { imageState
                                            | imageOffsetX = imageOffsetX
                                            , imageOffsetXUser = imageOffsetXString
                                        }
                              }
                            , getCanvasBoundingRect ()
                            )

                _ ->
                    ( model, Cmd.none )

        ImageOffsetYChange imageOffsetYString ->
            case model.documentState of
                Ready imageState ->
                    case String.toFloat imageOffsetYString of
                        Nothing ->
                            ( { model
                                | documentState =
                                    Ready
                                        { imageState
                                            | imageOffsetYUser = imageOffsetYString
                                        }
                              }
                            , Cmd.none
                            )

                        Just imageOffsetY ->
                            ( { model
                                | documentState =
                                    Ready
                                        { imageState
                                            | imageOffsetY = imageOffsetY
                                            , imageOffsetYUser = imageOffsetYString
                                        }
                              }
                            , getCanvasBoundingRect ()
                            )

                _ ->
                    ( model, Cmd.none )

        CanvasScaleChange scaleString ->
            case model.documentState of
                Ready imageState ->
                    case String.toFloat scaleString of
                        Nothing ->
                            ( { model
                                | documentState =
                                    Ready
                                        { imageState
                                            | canvasScaleUser = scaleString
                                        }
                              }
                            , Cmd.none
                            )

                        Just canvasScale ->
                            ( { model
                                | documentState =
                                    Ready
                                        { imageState
                                            | canvasScale = canvasScale
                                            , canvasScaleUser = scaleString
                                        }
                              }
                            , getCanvasBoundingRect ()
                            )

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
                            mouseClickData.x - model.canvasBoundingRect.x - (imageState.imageOffsetX * imageState.canvasScale)

                        y =
                            mouseClickData.y - model.canvasBoundingRect.y - (imageState.imageOffsetY * imageState.canvasScale)

                        tileSize =
                            16 * imageState.canvasScale

                        row =
                            x / tileSize |> floor

                        col =
                            y / tileSize |> floor

                        -- _ = Debug.log "x / tileSize" (x / tileSize)
                        -- _ = Debug.log "x y" (x, y)
                        -- _ = Debug.log "row col" (row, col)
                        maybeRowAndColIndex =
                            Vector29.intToIndex col
                                |> Maybe.andThen
                                    (\rowIndexValue ->
                                        Vector31.intToIndex row
                                            |> Maybe.map (\colIndexValue -> ( rowIndexValue, colIndexValue ))
                                    )
                    in
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
    Decode.map4 JSONDecodedDocument
        (Decode.field "imageOffsetX" Decode.float)
        (Decode.field "imageOffsetY" Decode.float)
        (Decode.field "canvasScale" Decode.float)
        (Decode.field "tiles" (Decode.list (Decode.list Decode.string)))


type alias JSONDecodedDocument =
    { imageOffsetX : Float
    , imageOffsetY : Float
    , canvasScale : Float
    , tiles : List (List String)
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


worldToList : Vector29.Vector29 (Vector31.Vector31 TileType) -> List (List String)
worldToList world =
    Vector29.map rowTilesToRowStrings world
        |> Vector29.map rowStringsToListStrings
        |> Vector29.toList


listToWorld : List (List String) -> Vector29.Vector29 (Vector31.Vector31 TileType)
listToWorld list =
    case Vector29.fromList list of
        Nothing ->
            defaultWorld

        Just ( leftOvers, vector30 ) ->
            if List.length leftOvers > 0 then
                defaultWorld

            else
                Vector29.map
                    (\row ->
                        case Vector31.fromList row of
                            Nothing ->
                                Vector31.repeat NotWalkable

                            Just ( leftOverCols, vector31 ) ->
                                if List.length leftOverCols > 0 then
                                    Vector31.repeat NotWalkable

                                else
                                    Vector31.map
                                        (\tileTypeString ->
                                            case tileTypeString of
                                                "Walkable" ->
                                                    Walkable

                                                _ ->
                                                    NotWalkable
                                        )
                                        vector31
                    )
                    vector30


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
    div [ Attr.class "w-full" ]
        [ header [ Attr.class "antialiased" ]
            [ nav [ Attr.class "bg-white border-gray-200 px-4 lg:px-6 py-2.5 dark:bg-gray-800" ]
                [ div [ Attr.class "flex flex-wrap justify-between items-center" ]
                    [ div [ Attr.class "flex justify-start items-center" ]
                        [ a [ href "https://github.com/JesterXL/final-cow-legend-level-editor", Attr.class "flex mr-4" ]
                            [ img [ src "logo.jpg", Attr.class "mr-3 h-8" ] []
                            , div [ Attr.class "flex flex-row items-baseline" ]
                                [ span [ Attr.class "text-2xl font-semibold whitespace-nowrap dark:text-white" ] [ text "Level Editor" ]
                                , span [ Attr.class "text-2xl font-semibold text-xs whitespace-nowrap dark:text-white pl-2" ] [ text "v1.0" ]
                                ]
                            ]
                        ]
                    , div
                        []
                        [ button
                            [ Attr.type_ "button"
                            , Attr.class "text-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-sm p-2.5 text-center inline-flex items-center me-2 dark:bg-blue-600 dark:hover:bg-blue-700 dark:focus:ring-blue-800"
                            ]
                            [ Svg.svg
                                [ SvgAttr.width "24px"
                                , SvgAttr.height "24px"
                                , SvgAttr.viewBox "0 0 36 36"
                                , Attr.attribute "aria-hidden" "true"
                                , Attr.attribute "role" "img"
                                , SvgAttr.class "iconify iconify--twemoji"
                                , SvgAttr.preserveAspectRatio "xMidYMid meet"
                                ]
                                [ Svg.path
                                    [ SvgAttr.fill "#3B88C3"
                                    , SvgAttr.d "M14.57 27.673c2.814-1.692 6.635-3.807 9.899-7.071c7.03-7.029 12.729-16.97 11.314-18.385C34.369.803 24.428 6.502 17.398 13.531c-3.265 3.265-5.379 7.085-7.071 9.899l4.243 4.243z"
                                    ]
                                    []
                                , Svg.path
                                    [ SvgAttr.fill "#C1694F"
                                    , SvgAttr.d "M.428 34.744s7.071 1.414 12.021-3.536c2.121-2.121 2.121-4.949 2.121-4.949l-2.829-2.829s-3.535.708-4.95 2.122c-1.414 1.414-2.518 4.232-2.888 5.598c-.676 2.502-3.475 3.594-3.475 3.594z"
                                    ]
                                    []
                                , Svg.path
                                    [ SvgAttr.fill "#CCD6DD"
                                    , SvgAttr.d "M17.882 25.328l-5.168-5.168c-.391-.391-.958-.326-1.27.145l-1.123 1.705c-.311.471-.271 1.142.087 1.501l4.122 4.123c.358.358 1.03.397 1.501.087l1.705-1.124c.472-.311.536-.878.146-1.269z"
                                    ]
                                    []
                                , Svg.path
                                    [ SvgAttr.fill "#A0041E"
                                    , SvgAttr.d "M11.229 32.26c-1.191.769-1.826.128-1.609-.609c.221-.751-.12-1.648-1.237-1.414c-1.117.233-1.856-.354-1.503-1.767c.348-1.393-1.085-1.863-1.754-.435c-.582 1.16-1.017 2.359-1.222 3.115c-.677 2.503-3.476 3.595-3.476 3.595s5.988 1.184 10.801-2.485z"
                                    ]
                                    []
                                ]
                            , span
                                [ Attr.class "sr-only"
                                ]
                                [ text "Icon description" ]
                            ]
                        ]
                    , div [ Attr.class "flex items-center lg:order-2" ]
                        [ button [ Attr.class "hidden sm:inline-flex items-center justify-center text-white bg-primary-700 hover:bg-primary-800 focus:ring-4 focus:ring-primary-300 font-medium rounded-lg text-xs px-3 py-1.5 mr-2 dark:bg-primary-600 dark:hover:bg-primary-700 focus:outline-none dark:focus:ring-primary-800", onClick OpenImage ] [ text "Open Image" ]
                        , button [ Attr.class "hidden sm:inline-flex items-center justify-center text-white bg-primary-700 hover:bg-primary-800 focus:ring-4 focus:ring-primary-300 font-medium rounded-lg text-xs px-3 py-1.5 mr-2 dark:bg-primary-600 dark:hover:bg-primary-700 focus:outline-none dark:focus:ring-primary-800", onClick OpenFile ] [ text "Open File" ]
                        ]
                    ]
                ]
            , case model.documentState of
                Ready { jsonString, imageBytes, base64Image, sprite, imageOffsetX, imageOffsetXUser, imageOffsetY, imageOffsetYUser, tiles, canvasScale, canvasScaleUser } ->
                    let
                        widthAndHeight =
                            Canvas.Texture.dimensions sprite
                    in
                    div
                        [ Attr.class "flex flex-row flex-nowrap" ]
                        [ div [ Attr.class "grow" ]
                            [ Canvas.toHtmlWith
                                { width = round widthAndHeight.width
                                , height = round widthAndHeight.height
                                , textures = []
                                }
                                [ Attr.class "block pixel-art", on "click" (Decode.map MouseClick mouseClickDecoder) ]
                                [ shapes
                                    [ CanvasSettings.fill (Color.rgb 0.85 0.92 1) ]
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
                        , div [ Attr.class "w-[360px] h-screen p-4 overflow-y-auto bg-white dark:bg-gray-800" ]
                            [ h5
                                [ Attr.class "inline-flex items-center mb-6 text-sm font-semibold text-gray-500 uppercase dark:text-gray-400" ]
                                [ text "Properties" ]
                            , div [ Attr.class "text-white" ]
                                [ div [ Attr.class "space-y-4" ]
                                    [ div []
                                        [ label
                                            [ Attr.class "block mb-2 text-sm font-medium text-gray-900 dark:text-white" ]
                                            [ text "Offset X:" ]
                                        , input
                                            [ Attr.class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                            , onInput ImageOffsetXChange
                                            , value imageOffsetXUser
                                            ]
                                            []
                                        ]
                                    , div []
                                        [ label
                                            [ Attr.class "block mb-2 text-sm font-medium text-gray-900 dark:text-white" ]
                                            [ text "Offset Y:" ]
                                        , input
                                            [ Attr.class "bbg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                            , onInput ImageOffsetYChange
                                            , value imageOffsetYUser
                                            ]
                                            []
                                        ]
                                    , div []
                                        [ label [ Attr.class "block mb-2 text-sm font-medium text-gray-900 dark:text-white" ] [ text "Scale:" ]
                                        , input
                                            [ Attr.class "bbg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                            , onInput CanvasScaleChange
                                            , value canvasScaleUser
                                            ]
                                            []
                                        , input
                                            [ Attr.class "mt-4 w-full h-2 bg-gray-200 rounded-lg appearance-none cursor-pointer dark:bg-gray-700"
                                            , Attr.type_ "range"
                                            , onInput CanvasScaleChange
                                            , Attr.min "1"
                                            , Attr.max "6"
                                            , Attr.step "0.1"
                                            , value (String.fromFloat canvasScale)
                                            ]
                                            []

                                        -- Attr.class="w-full h-2 bg-gray-200 rounded-lg appearance-none cursor-pointer dark:bg-gray-700">
                                        ]
                                    , div []
                                        [ label [ Attr.class "block mb-2 text-sm font-medium text-gray-900 dark:text-white" ] [ text "Map JSON:" ]
                                        , textarea
                                            [ rows 4, Attr.class "block p-2.5 w-full text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500" ]
                                            [ text jsonString ]
                                        ]
                                    , button [ onClick (SaveLevel { jsonString = jsonString, imageBytes = imageBytes, imageOffsetX = imageOffsetX, imageOffsetY = imageOffsetY, canvasScale = canvasScale, tiles = tiles }) ] [ text "Save" ]
                                    ]
                                ]
                            ]
                        ]

                _ ->
                    div
                        [ Attr.class "flex flex-row flex-nowrap" ]
                        [ div [ Attr.class "grow" ]
                            [ Canvas.toHtmlWith
                                { width = round 600
                                , height = round 600
                                , textures = []
                                }
                                [ Attr.class "block pixel-art" ]
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
        (Vector29.indexedMap
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
            |> Vector29.toList
            |> List.concatMap
                (\cell -> cell)
        )
    ]


drawCell : Row -> Col -> TileType -> List Canvas.Renderable
drawCell (Row row) (Col col) tileType =
    let
        rowInt =
            Vector29.indexToInt row

        colInt =
            Vector31.indexToInt col
    in
    [ shapes
        [ if tileType == Walkable then
            CanvasSettings.fill Color.green

          else
            CanvasSettings.fill Color.red
        ]
        [ rect ( Basics.toFloat colInt * 16, Basics.toFloat rowInt * 16 ) 16 16 ]
    , shapes [ CanvasSettings.stroke Color.lightGreen ] [ rect ( Basics.toFloat colInt * 16, Basics.toFloat rowInt * 16 ) 16 16 ]
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
