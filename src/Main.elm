module Main exposing (main)

import AStar exposing (findPath, straightLineCost)
import Animator exposing (color)
import Array exposing (Array)
import Browser
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onKeyUp, onVisibilityChange)
import Canvas exposing (Point, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Canvas.Texture exposing (sprite)
import Color
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random
import Set


type alias Model =
    { documentState : DocumentState
    }


type DocumentState
    = WaitingOnUser
    | Loading
    | Ready
    | Failed String


initialModel : Model
initialModel =
    { documentState = WaitingOnUser
    }


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "flex flex-row" ]
        [ Canvas.toHtmlWith
            { width = 300
            , height = 300
            , textures = []
            }
            [ class "block pixel-art" ]
            [ shapes
                [ fill (Color.rgb 0.85 0.92 1) ]
                [ rect ( 0, 0 ) 300 300 ]
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
