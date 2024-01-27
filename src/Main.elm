port module Main exposing (main)

import AStar exposing (findPath, straightLineCost)
import Animator exposing (color)
import Array exposing (Array)
import Base64.Encode as Base64Encode
import Browser
import Browser.Events as BrowserEvents exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onKeyUp, onVisibilityChange)
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
import Html.Events exposing (on, onClick, onInput, onMouseDown, onMouseUp)
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
        , paintMode : PaintMode
        }
    | Failed String


type PaintMode
    = Toggle
    | Brush Bool
    | Erase
    | FillWalkable
    | FillUnwalkable


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
    | OnMouseDown
    | OnMouseUp
    | MouseMoved Float Float
    | StopMouseMove
    | CanvasBoundingRectLoaded Decode.Value
    | ChangePaintMode PaintMode


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
                                        , paintMode = Toggle
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
                    case imageState.paintMode of
                        Toggle ->
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

                _ ->
                    ( model, Cmd.none )

        OnMouseDown ->
            case model.documentState of
                Ready imageState ->
                    case imageState.paintMode of
                        Brush False ->
                            ( { model
                                | documentState =
                                    Ready { imageState | paintMode = Brush True }
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnMouseUp ->
            case model.documentState of
                Ready imageState ->
                    case imageState.paintMode of
                        Brush True ->
                            ( { model
                                | documentState =
                                    Ready { imageState | paintMode = Brush False }
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseMoved x y ->
            case model.documentState of
                Ready imageState ->
                    case imageState.paintMode of
                        Brush True ->
                            let
                                mouseMoveX =
                                    x - model.canvasBoundingRect.x - (imageState.imageOffsetX * imageState.canvasScale)

                                mouseMoveY =
                                    y - model.canvasBoundingRect.y - (imageState.imageOffsetY * imageState.canvasScale)

                                tileSize =
                                    16 * imageState.canvasScale

                                row =
                                    mouseMoveX / tileSize |> floor

                                col =
                                    mouseMoveY / tileSize |> floor

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

                                        updatedWorld =
                                            setCell (Row rowIndex) (Col colIndex) Walkable imageState.tiles
                                    in
                                    ( { model | documentState = Ready { imageState | tiles = updatedWorld } }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StopMouseMove ->
            case model.documentState of
                Ready imageState ->
                    case imageState.paintMode of
                        Brush pressed ->
                            let
                                _ =
                                    Debug.log "stop mouse move, pressed" pressed
                            in
                            ( { model
                                | documentState =
                                    Ready { imageState | paintMode = Brush False }
                              }
                            , Cmd.none
                            )

                        _ ->
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

        ChangePaintMode newMode ->
            case model.documentState of
                Ready currentProps ->
                    ( { model
                        | documentState = Ready { currentProps | paintMode = newMode }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


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
                    , paintButtons model
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
                                [ Attr.class "block pixel-art"
                                , on "click" (Decode.map MouseClick mouseClickDecoder)
                                , onMouseDown OnMouseDown
                                , onMouseUp OnMouseUp
                                ]
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


paintButtons : Model -> Html Msg
paintButtons model =
    div
        []
        [ button
            [ Attr.type_ "button"
            , Attr.class (getPaintButtonSelected model.documentState Toggle)
            , Attr.disabled (getPaintButtonDisabled model.documentState)
            , onClick (ChangePaintMode Toggle)
            ]
            [ Svg.svg
                [ SvgAttr.width paintIconWidth
                , SvgAttr.height paintIconHeight
                , SvgAttr.viewBox "0 0 1024 1024"
                , SvgAttr.class "icon"
                , SvgAttr.version "1.1"
                ]
                [ Svg.path
                    [ SvgAttr.d "M884.973779 195.009199c7.324876-7.199962 11.395844-16.966804 11.456253-27.35924 0-10.581855-4.317724-20.973267-11.8955-28.552066l-32.179692-32.241125c-14.96306-15.027565-41.069064-15.338826-55.846801-0.564161l-1.12525 1.128322c-10.956597-6.074712-23.790999-7.702689-36.000832-5.698946-2.003744-0.62457-3.881549-1.499992-6.07164-1.689411a335.185243 335.185243 0 0 0-26.984497-1.063817c-164.097077 0-218.564705 127.156318-241.792568 181.500056-3.567216 8.202345-7.574704 17.530965-9.517014 20.473611-3.317388 4.881885-13.459996 15.274321-44.700785 29.737726-3.506807 1.627978-6.449454 4.006463-9.014287 6.574367-16.091382 16.08831 3.693154 36.626426 20.784872 30.426801 23.541171-9.578447 49.647176-20.909786 68.366871-49.585743 3.06756-4.756971 6.824196-13.02075 11.520758-23.538099 21.848689-49.275505 66.988721-150.258243 184.69253-154.768458L402.078743 444.436879c-91.344905 91.597805-170.35404 183.067624-222.943862 258.070296-0.374742 0.62457-0.938903 1.503064-1.37815 2.318077-36.75134 52.779241-56.221543 91.405315-59.603436 118.014046-0.938903 7.138528 0.689075 13.649415 3.317388 19.28488l-21.723775 21.723775c-7.44979 7.574704-7.513271 19.723103 0.064505 27.297807 7.44979 7.513271 19.65655 7.513271 27.234326-0.064505l21.663366-21.663366c4.756971 2.318076 9.953189 4.135473 15.773977 4.135473 1.003408 0 2.882237-0.127986 3.881549-0.2529 26.923065-3.317388 65.863471-22.977011 118.957045-59.978178 0.813989-0.499656 1.564497-1.000336 2.313981-1.499992 74.441583-52.278561 165.222327-130.663126 255.941638-221.068104a16.615611 16.615611 0 0 0 1.564497-1.37815l328.315997-328.376406c10.081175-10.081175 15.591726-23.47769 15.591725-37.815157a55.407554 55.407554 0 0 0-7.075047-27.109411l0.999312-1.065865z"
                    , SvgAttr.fill "#27323A"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M510.014686 404.682484l204.979795-204.976723 76.756587 76.755563L586.70984 481.439071z"
                    , SvgAttr.fill "#79CCBF"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M463.67978 451.011247l23.5381-23.537076 76.757611 76.756587-23.5381 23.5381zM260.961748 772.87779c-51.086758 35.687523-78.195146 47.207257-91.344905 50.900411-0.625594-0.689075-1.253236-1.375078-1.942311-2.003744 3.696226-13.085254 15.216984-40.130161 50.964916-91.344905 0.62457-0.750508 1.063817-1.564497 1.503064-2.378486 0.249828-0.374742 0.561089-0.750508 0.750508-1.063817l43.512055 43.512055c-0.564161 0.374742-1.188731 0.878494-1.814325 1.253236-0.564161 0.375766-1.129346 0.750508-1.629002 1.12525z"
                    , SvgAttr.fill "#FFFFFF"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M512.896924 555.130145c-77.758971 77.630985-155.394052 146.126866-222.132946 196.213289l-50.650583-50.650584C289.948966 634.265218 358.380341 556.630137 436.200746 478.495401l4.756971-4.756972 76.759659 76.756588-3.25698 3.256979c-0.564161 0.439247-1.063817 0.938903-1.563472 1.378149z"
                    , SvgAttr.fill "#F4CE73"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M834.198282 211.476347l6.135121 6.260035c2.128658 2.064153 2.44299 4.317724 2.44299 5.507479 0 1.442654-0.439247 2.631385-1.439583 3.571312l-26.85856 26.85856-76.695154-76.759659 26.795079-26.85856c1.692482-1.689411 4.320796-3.881549 9.267186 1.063817l5.949798 6.010206c9.453533 9.3921 24.727854 9.3921 34.119955 0l10.267522-10.203017 20.284192 20.348697-10.142608 10.203017c-9.390052 9.328619-9.453533 24.541507-0.125938 33.998113z"
                    , SvgAttr.fill "#FFFFFF"
                    ]
                    []
                ]
            , span
                [ Attr.class "sr-only"
                ]
                [ text "Toggle Tiles" ]
            ]
        , button
            [ Attr.type_ "button"
            , Attr.class (getPaintButtonSelected model.documentState (Brush False))
            , Attr.disabled (getPaintButtonDisabled model.documentState)
            , onClick (ChangePaintMode (Brush False))
            ]
            [ Svg.svg
                [ SvgAttr.width paintIconWidth
                , SvgAttr.height paintIconHeight
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
                    [ SvgAttr.fill "#04A019"
                    , SvgAttr.d "M11.229 32.26c-1.191.769-1.826.128-1.609-.609c.221-.751-.12-1.648-1.237-1.414c-1.117.233-1.856-.354-1.503-1.767c.348-1.393-1.085-1.863-1.754-.435c-.582 1.16-1.017 2.359-1.222 3.115c-.677 2.503-3.476 3.595-3.476 3.595s5.988 1.184 10.801-2.485z"
                    ]
                    []
                ]
            , span
                [ Attr.class "sr-only"
                ]
                [ text "Paint Walkable Tiles" ]
            ]
        , button
            [ Attr.type_ "button"
            , Attr.class (getPaintButtonSelected model.documentState Erase)
            , Attr.disabled (getPaintButtonDisabled model.documentState)
            , onClick (ChangePaintMode Erase)
            ]
            [ Svg.svg
                [ SvgAttr.width paintIconWidth
                , SvgAttr.height paintIconHeight
                , SvgAttr.version "1.1"
                , SvgAttr.id "Layer_1"
                , SvgAttr.viewBox "0 0 506 506"
                , SvgAttr.xmlSpace "preserve"
                ]
                [ Svg.path
                    [ SvgAttr.style "fill:#DF5C4E;"
                    , SvgAttr.d "M193,215.7c-0.8-0.8-0.8-2,0-2.8s2-0.8,2.8,0l150,150l140-140c9.2-9.2,14.4-21.6,14.4-35.2\n\ts-5.2-25.6-14.4-35.2l-106-105.6c-9.2-9.2-21.6-14.4-35.2-14.4s-25.6,5.2-35.2,14.4L20.2,335.7C1,355.3,1,386.9,20.2,406.1\n\tl67.6,67.6h147.6L343,366.1L193,215.7z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.style "fill:#33A5D2;"
                    , SvgAttr.d "M193,215.7c-0.8-0.8-0.8-2,0-2.8s2-0.8,2.8,0l150,150l140-140c9.2-9.2,14.4-21.6,14.4-35.2\n\ts-5.2-25.6-14.4-35.2l-106-105.6c-9.2-9.2-21.6-14.4-35.2-14.4s-25.6,5.2-35.2,14.4L166.6,189.7l176,176L193,215.7z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.style "fill:#E36D60;"
                    , SvgAttr.d "M168.2,191.3L11.4,348.5c-6.8,13.2-7.2,29.2-0.8,43.2l179.2-179.2L168.2,191.3z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.style "fill:#73BADD;"
                    , SvgAttr.d "M189.4,212.5l176-175.6c-6.4-2.8-13.6-4.4-20.8-4.4c-8,0-15.6,2-22.4,5.6l-154,153.2L189.4,212.5z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M236.2,479.7H86.6c-1.2,0-2-0.4-2.8-1.2l-67.6-68c-21.6-21.6-21.6-57.2,0-78.8l2.8,2.8l-2.8-2.8L305.4,42.5\n\tc21.6-21.6,56.8-21.6,78.4,0l106,106c21.6,21.6,21.6,57.2,0,78.8L239,478.5C238.2,479.3,237.4,479.7,236.2,479.7z M88.2,471.7h146.4\n\tl249.6-250c8.8-8.8,14-20.8,14-33.6c0-12.8-4.8-24.8-14-33.6l-106-106c-18.4-18.4-48.8-18.4-67.2,0L21.8,337.7\n\tc-18.4,18.4-18.4,49.2,0,67.6L88.2,471.7z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M365,479.7H89c-2.4,0-4-1.6-4-4s1.6-4,4-4h276c2.4,0,4,1.6,4,4S367.4,479.7,365,479.7z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M345,368.9c-1.2,0-2-0.4-2.8-1.2L191.4,217.3c-1.6-1.6-1.6-4,0-5.6s4-1.6,5.6,0l150.8,150.4c1.6,1.6,1.6,4,0,5.6\n\tC347,368.5,346.2,368.9,345,368.9z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M445,463.7H253c-2.4,0-4-1.6-4-4s1.6-4,4-4h192c2.4,0,4,1.6,4,4S447.4,463.7,445,463.7z"
                    ]
                    []
                ]
            , span
                [ Attr.class "sr-only"
                ]
                [ text "Erase Multiple Tiles" ]
            ]
        , button
            [ Attr.type_ "button"
            , Attr.class (getPaintButtonSelected model.documentState FillWalkable)
            , Attr.disabled (getPaintButtonDisabled model.documentState)
            , onClick (ChangePaintMode FillWalkable)
            ]
            [ Svg.svg
                [ SvgAttr.width paintIconWidth
                , SvgAttr.height paintIconHeight
                , SvgAttr.version "1.1"
                , SvgAttr.id "Layer_1"
                , SvgAttr.viewBox "0 0 512 512"
                , SvgAttr.xmlSpace "preserve"
                ]
                [ Svg.g []
                    [ Svg.path
                        [ SvgAttr.style "fill:#25B99A;"
                        , SvgAttr.d "M26.069,405.333L26.069,405.333c-11.947-11.947-11.947-30.72,0-42.667l21.333-21.333l21.333,21.333\n\t\tc11.947,11.947,11.947,30.72,0,42.667l0,0C56.789,417.28,37.162,417.28,26.069,405.333"
                        ]
                        []
                    , Svg.path
                        [ SvgAttr.style "fill:#25B99A;"
                        , SvgAttr.d "M280.362,58.88c17.067,17.067-21.333,81.92-84.48,145.067s-128,101.547-145.067,84.48\n\t\tc-17.067-17.067,21.333-81.92,84.48-145.067S263.296,42.667,280.362,58.88"
                        ]
                        []
                    ]
                , Svg.path
                    [ SvgAttr.style "fill:#B9BDC5;"
                    , SvgAttr.d "M195.882,203.947c-63.147,63.147-128,101.547-145.067,84.48L261.589,499.2\n\tc17.067,17.067,81.92-21.333,145.067-84.48s101.547-128,84.48-145.067L280.362,58.88C296.576,75.947,259.029,140.8,195.882,203.947"
                    ]
                    []
                , Svg.g []
                    [ Svg.path
                        [ SvgAttr.style "fill:#556080;"
                        , SvgAttr.d "M313.642,220.16l90.453,90.453c3.413,3.413,9.387,3.413,11.947,0\n\t\tc32.427-34.987,46.933-69.12,34.987-80.213L340.096,120.32c10.24,10.24-0.853,40.107-28.16,71.68\n\t\tC305.962,200.533,305.962,212.48,313.642,220.16"
                        ]
                        []
                    , Svg.path
                        [ SvgAttr.style "fill:#556080;"
                        , SvgAttr.d "M155.776,343.04c-21.333,11.093-37.547,13.653-44.373,6.827l110.933,110.08\n\t\tc6.827,6.827,20.48,1.707,40.96-10.24c5.12-2.56,5.973-9.387,1.707-13.653l-89.6-89.6\n\t\tC170.282,341.333,162.602,339.627,155.776,343.04"
                        ]
                        []
                    ]
                , Svg.path
                    [ SvgAttr.style "fill:#E8EDEE;"
                    , SvgAttr.d "M342.656,470.187c20.48-14.507,41.813-33.28,64-55.467c2.56-2.56,4.267-4.267,6.827-6.827\n\tL202.709,197.12c-2.56,2.56-4.267,4.267-6.827,6.827c-22.187,22.187-43.52,40.96-64,55.467L342.656,470.187z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.style "fill:#6C797A;"
                    , SvgAttr.d "M273.536,256c0,18.773-15.36,34.133-34.133,34.133s-34.133-15.36-34.133-34.133\n\ts15.36-34.133,34.133-34.133S273.536,237.227,273.536,256"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.style "fill:#556080;"
                    , SvgAttr.d "M183.936,0c-34.987,0-64,29.013-64,64v95.573c5.12-5.12,10.24-11.093,15.36-16.213\n\tc0.853-0.853,0.853-0.853,1.707-1.707V64c0-25.6,21.333-46.933,46.933-46.933S230.869,38.4,230.869,64v192h17.067V64\n\tC247.936,29.013,218.922,0,183.936,0"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M47.402,422.4c-10.24,0-19.627-3.413-27.307-11.093l0,0c-15.36-15.36-15.36-39.253,0-54.613l21.333-21.333\n\tc3.413-3.413,8.533-3.413,11.947,0l21.333,21.333C81.536,363.52,85.802,373.76,85.802,384c0,10.24-4.267,20.48-11.093,27.307\n\tC67.029,418.987,56.789,422.4,47.402,422.4z M32.042,399.36c8.533,8.533,22.187,8.533,30.72,0c3.413-4.267,5.973-9.387,5.973-15.36\n\tc0-5.973-2.56-11.093-5.973-15.36l-15.36-15.36l-15.36,15.36C23.509,377.173,23.509,390.827,32.042,399.36L32.042,399.36z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M61.909,300.373c-6.827,0-12.8-1.707-17.067-5.973c-23.893-23.893,24.747-97.28,84.48-157.013s133.12-108.373,157.013-84.48\n\tl0,0c23.893,23.893-24.747,97.28-84.48,157.013C152.362,259.413,94.336,300.373,61.909,300.373z M268.416,63.147\n\tc-17.067,0-66.56,24.747-127.147,86.187c-69.973,70.827-92.16,124.587-84.48,133.12c8.533,8.533,62.293-14.507,133.12-84.48\n\tc69.973-70.827,92.16-124.587,84.48-133.12C273.536,64,270.976,63.147,268.416,63.147z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M273.536,512c-7.68,0-12.8-2.56-17.067-6.827L44.842,294.4l11.947-11.947c8.533,8.533,62.293-14.507,133.12-84.48\n\tc69.973-70.827,92.16-124.587,84.48-133.12l11.947-11.947l210.773,211.627c11.093,11.093,7.68,33.28-11.093,64.853\n\tc-16.213,28.16-42.667,60.587-73.387,91.307c-30.72,30.72-64,57.173-91.307,73.387C301.696,506.027,285.482,512,273.536,512z\n\t M73.856,299.52l194.56,194.56c3.413,3.413,17.92,2.56,44.373-13.653c26.453-15.36,58.027-40.96,87.893-70.827\n\tc69.973-69.973,93.013-124.587,84.48-133.12L290.602,81.92c-7.68,33.28-45.227,84.48-88.747,128l0,0\n\tc-30.72,30.72-64,57.173-91.307,73.387C96.042,291.84,84.096,296.96,73.856,299.52z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M247.936,256h-17.067V64c0-25.6-21.333-46.933-46.933-46.933S137.002,38.4,137.002,64v87.04h-17.067V64\n\tc0-34.987,29.013-64,64-64s64,29.013,64,64V256z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M239.402,298.667c-11.093,0-22.187-4.267-29.867-12.8c-16.213-17.067-16.213-43.52,0-60.587l11.947,11.947\n\tc-10.24,10.24-10.24,26.453,0,35.84c10.24,10.24,26.453,10.24,35.84,0c5.12-5.12,7.68-11.093,7.68-17.92s-2.56-13.653-7.68-17.92\n\tl11.947-11.947c7.68,7.68,12.8,18.773,12.8,29.867c0,11.093-4.267,22.187-12.8,29.867\n\tC261.589,294.4,250.496,298.667,239.402,298.667z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M410.069,321.707c-4.267,0-8.533-1.707-11.947-5.12l-90.453-90.453c-10.24-10.24-11.093-27.307-1.707-39.253\n\tc29.013-33.28,32.427-56.32,28.16-60.587l11.947-11.947l110.933,110.08c17.92,17.92-4.267,58.88-34.987,92.16\n\tC419.456,320,415.189,321.707,410.069,321.707L410.069,321.707z M351.189,143.36c-4.267,17.067-17.92,37.547-32.427,54.613\n\tc-4.267,5.12-4.267,11.947,0,16.213l91.307,90.453c33.28-35.84,40.96-63.147,35.84-68.267L351.189,143.36z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M230.869,471.04c-5.12,0-10.24-1.707-14.507-5.973l-110.933-110.08l0,0l11.947-11.947l0,0\n\tc2.56,2.56,11.947,3.413,34.133-7.68l0,0c10.24-5.12,22.187-3.413,29.867,4.267l89.6,89.6c3.413,3.413,5.973,9.387,5.12,14.507\n\tc-0.853,5.12-4.267,10.24-8.533,12.8C255.616,463.36,241.962,471.04,230.869,471.04z M134.442,360.96l93.867,93.013\n\tc2.56,2.56,12.8-1.707,30.72-11.947l-89.6-89.6c-2.56-2.56-6.827-3.413-10.24-1.707C149.802,355.84,142.122,359.253,134.442,360.96z\n\t"
                    ]
                    []
                ]
            , span
                [ Attr.class "sr-only"
                ]
                [ text "Make All Tiles Walkable" ]
            ]
        , button
            [ Attr.type_ "button"
            , Attr.class (getPaintButtonSelected model.documentState FillUnwalkable)
            , Attr.disabled (getPaintButtonDisabled model.documentState)
            , onClick (ChangePaintMode FillUnwalkable)
            ]
            [ Svg.svg
                [ SvgAttr.width paintIconWidth
                , SvgAttr.height paintIconHeight
                , SvgAttr.version "1.1"
                , SvgAttr.id "Layer_1"
                , SvgAttr.viewBox "0 0 512 512"
                , SvgAttr.xmlSpace "preserve"
                ]
                [ Svg.g []
                    [ Svg.path
                        [ SvgAttr.style "fill:#B92546;"
                        , SvgAttr.d "M26.069,405.333L26.069,405.333c-11.947-11.947-11.947-30.72,0-42.667l21.333-21.333l21.333,21.333\n\t\tc11.947,11.947,11.947,30.72,0,42.667l0,0C56.789,417.28,37.162,417.28,26.069,405.333"
                        ]
                        []
                    , Svg.path
                        [ SvgAttr.style "fill:#B92546;"
                        , SvgAttr.d "M280.362,58.88c17.067,17.067-21.333,81.92-84.48,145.067s-128,101.547-145.067,84.48\n\t\tc-17.067-17.067,21.333-81.92,84.48-145.067S263.296,42.667,280.362,58.88"
                        ]
                        []
                    ]
                , Svg.path
                    [ SvgAttr.style "fill:#B9BDC5;"
                    , SvgAttr.d "M195.882,203.947c-63.147,63.147-128,101.547-145.067,84.48L261.589,499.2\n\tc17.067,17.067,81.92-21.333,145.067-84.48s101.547-128,84.48-145.067L280.362,58.88C296.576,75.947,259.029,140.8,195.882,203.947"
                    ]
                    []
                , Svg.g []
                    [ Svg.path
                        [ SvgAttr.style "fill:#556080;"
                        , SvgAttr.d "M313.642,220.16l90.453,90.453c3.413,3.413,9.387,3.413,11.947,0\n\t\tc32.427-34.987,46.933-69.12,34.987-80.213L340.096,120.32c10.24,10.24-0.853,40.107-28.16,71.68\n\t\tC305.962,200.533,305.962,212.48,313.642,220.16"
                        ]
                        []
                    , Svg.path
                        [ SvgAttr.style "fill:#556080;"
                        , SvgAttr.d "M155.776,343.04c-21.333,11.093-37.547,13.653-44.373,6.827l110.933,110.08\n\t\tc6.827,6.827,20.48,1.707,40.96-10.24c5.12-2.56,5.973-9.387,1.707-13.653l-89.6-89.6\n\t\tC170.282,341.333,162.602,339.627,155.776,343.04"
                        ]
                        []
                    ]
                , Svg.path
                    [ SvgAttr.style "fill:#E8EDEE;"
                    , SvgAttr.d "M342.656,470.187c20.48-14.507,41.813-33.28,64-55.467c2.56-2.56,4.267-4.267,6.827-6.827\n\tL202.709,197.12c-2.56,2.56-4.267,4.267-6.827,6.827c-22.187,22.187-43.52,40.96-64,55.467L342.656,470.187z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.style "fill:#6C797A;"
                    , SvgAttr.d "M273.536,256c0,18.773-15.36,34.133-34.133,34.133s-34.133-15.36-34.133-34.133\n\ts15.36-34.133,34.133-34.133S273.536,237.227,273.536,256"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.style "fill:#556080;"
                    , SvgAttr.d "M183.936,0c-34.987,0-64,29.013-64,64v95.573c5.12-5.12,10.24-11.093,15.36-16.213\n\tc0.853-0.853,0.853-0.853,1.707-1.707V64c0-25.6,21.333-46.933,46.933-46.933S230.869,38.4,230.869,64v192h17.067V64\n\tC247.936,29.013,218.922,0,183.936,0"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M47.402,422.4c-10.24,0-19.627-3.413-27.307-11.093l0,0c-15.36-15.36-15.36-39.253,0-54.613l21.333-21.333\n\tc3.413-3.413,8.533-3.413,11.947,0l21.333,21.333C81.536,363.52,85.802,373.76,85.802,384c0,10.24-4.267,20.48-11.093,27.307\n\tC67.029,418.987,56.789,422.4,47.402,422.4z M32.042,399.36c8.533,8.533,22.187,8.533,30.72,0c3.413-4.267,5.973-9.387,5.973-15.36\n\tc0-5.973-2.56-11.093-5.973-15.36l-15.36-15.36l-15.36,15.36C23.509,377.173,23.509,390.827,32.042,399.36L32.042,399.36z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M61.909,300.373c-6.827,0-12.8-1.707-17.067-5.973c-23.893-23.893,24.747-97.28,84.48-157.013s133.12-108.373,157.013-84.48\n\tl0,0c23.893,23.893-24.747,97.28-84.48,157.013C152.362,259.413,94.336,300.373,61.909,300.373z M268.416,63.147\n\tc-17.067,0-66.56,24.747-127.147,86.187c-69.973,70.827-92.16,124.587-84.48,133.12c8.533,8.533,62.293-14.507,133.12-84.48\n\tc69.973-70.827,92.16-124.587,84.48-133.12C273.536,64,270.976,63.147,268.416,63.147z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M273.536,512c-7.68,0-12.8-2.56-17.067-6.827L44.842,294.4l11.947-11.947c8.533,8.533,62.293-14.507,133.12-84.48\n\tc69.973-70.827,92.16-124.587,84.48-133.12l11.947-11.947l210.773,211.627c11.093,11.093,7.68,33.28-11.093,64.853\n\tc-16.213,28.16-42.667,60.587-73.387,91.307c-30.72,30.72-64,57.173-91.307,73.387C301.696,506.027,285.482,512,273.536,512z\n\t M73.856,299.52l194.56,194.56c3.413,3.413,17.92,2.56,44.373-13.653c26.453-15.36,58.027-40.96,87.893-70.827\n\tc69.973-69.973,93.013-124.587,84.48-133.12L290.602,81.92c-7.68,33.28-45.227,84.48-88.747,128l0,0\n\tc-30.72,30.72-64,57.173-91.307,73.387C96.042,291.84,84.096,296.96,73.856,299.52z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M247.936,256h-17.067V64c0-25.6-21.333-46.933-46.933-46.933S137.002,38.4,137.002,64v87.04h-17.067V64\n\tc0-34.987,29.013-64,64-64s64,29.013,64,64V256z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M239.402,298.667c-11.093,0-22.187-4.267-29.867-12.8c-16.213-17.067-16.213-43.52,0-60.587l11.947,11.947\n\tc-10.24,10.24-10.24,26.453,0,35.84c10.24,10.24,26.453,10.24,35.84,0c5.12-5.12,7.68-11.093,7.68-17.92s-2.56-13.653-7.68-17.92\n\tl11.947-11.947c7.68,7.68,12.8,18.773,12.8,29.867c0,11.093-4.267,22.187-12.8,29.867\n\tC261.589,294.4,250.496,298.667,239.402,298.667z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M410.069,321.707c-4.267,0-8.533-1.707-11.947-5.12l-90.453-90.453c-10.24-10.24-11.093-27.307-1.707-39.253\n\tc29.013-33.28,32.427-56.32,28.16-60.587l11.947-11.947l110.933,110.08c17.92,17.92-4.267,58.88-34.987,92.16\n\tC419.456,320,415.189,321.707,410.069,321.707L410.069,321.707z M351.189,143.36c-4.267,17.067-17.92,37.547-32.427,54.613\n\tc-4.267,5.12-4.267,11.947,0,16.213l91.307,90.453c33.28-35.84,40.96-63.147,35.84-68.267L351.189,143.36z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M230.869,471.04c-5.12,0-10.24-1.707-14.507-5.973l-110.933-110.08l0,0l11.947-11.947l0,0\n\tc2.56,2.56,11.947,3.413,34.133-7.68l0,0c10.24-5.12,22.187-3.413,29.867,4.267l89.6,89.6c3.413,3.413,5.973,9.387,5.12,14.507\n\tc-0.853,5.12-4.267,10.24-8.533,12.8C255.616,463.36,241.962,471.04,230.869,471.04z M134.442,360.96l93.867,93.013\n\tc2.56,2.56,12.8-1.707,30.72-11.947l-89.6-89.6c-2.56-2.56-6.827-3.413-10.24-1.707C149.802,355.84,142.122,359.253,134.442,360.96z\n\t"
                    ]
                    []
                ]
            , span
                [ Attr.class "sr-only"
                ]
                [ text "Make All Tiles Unwalkable" ]
            ]
        ]


paintIconWidth : String
paintIconWidth =
    "20px"


paintIconHeight : String
paintIconHeight =
    "20px"


paintButtonOutlineStyles : String
paintButtonOutlineStyles =
    "text-blue-700 hover:text-white border border-blue-700 hover:bg-blue-800 focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-sm py-2.5 px-2.5 text-center me-2 mb-2 dark:border-blue-500 dark:text-blue-500 dark:hover:text-white dark:hover:bg-blue-500 dark:focus:ring-blue-800"


paintButtonSelectedStyles : String
paintButtonSelectedStyles =
    "text-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-sm p-2.5 text-center inline-flex items-center me-2 dark:bg-blue-600 dark:hover:bg-blue-700 dark:focus:ring-blue-800"


getPaintButtonSelected : DocumentState -> PaintMode -> String
getPaintButtonSelected documentState buttonMode =
    case documentState of
        Ready { paintMode } ->
            if paintMode == buttonMode then
                paintButtonSelectedStyles

            else
                paintButtonOutlineStyles

        _ ->
            paintButtonOutlineStyles


getPaintButtonDisabled : DocumentState -> Bool
getPaintButtonDisabled documentState =
    case documentState of
        Ready _ ->
            False

        _ ->
            True


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
    case model.documentState of
        Ready imageState ->
            case imageState.paintMode of
                Brush False ->
                    Sub.batch
                        [ onImageFromJavaScript ImageLoadedFromJavaScript
                        , onKeyDown keyDecoderPressed
                        , onKeyUp keyDecoderReleased
                        , Time.every (10 * 1000) Tick
                        , onCanvasBoundingRect CanvasBoundingRectLoaded
                        ]

                Brush True ->
                    Sub.batch
                        [ onImageFromJavaScript ImageLoadedFromJavaScript
                        , onKeyDown keyDecoderPressed
                        , onKeyUp keyDecoderReleased
                        , Time.every (10 * 1000) Tick
                        , onCanvasBoundingRect CanvasBoundingRectLoaded
                        , BrowserEvents.onMouseMove (Decode.map2 MouseMoved (Decode.field "pageX" Decode.float) (Decode.field "pageY" Decode.float))
                        , BrowserEvents.onMouseUp (Decode.succeed StopMouseMove)
                        ]

                _ ->
                    Sub.batch
                        [ onImageFromJavaScript ImageLoadedFromJavaScript
                        , onKeyDown keyDecoderPressed
                        , onKeyUp keyDecoderReleased
                        , Time.every (10 * 1000) Tick
                        , onCanvasBoundingRect CanvasBoundingRectLoaded
                        ]

        _ ->
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
