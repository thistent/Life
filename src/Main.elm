module Main exposing (..)

import Array
import Array2D exposing (Array2D)
import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import Keyboard as Kb
import Keyboard.Arrows as KbArrows
import List.Extra as LE
import Url


-- Main --


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        , onUrlChange = NewUrl
        , onUrlRequest = Link
        }



-- Model --


type PauseState
    = Running
    | Paused


type Liveness
    = Alive
    | Dead


type alias GameCell =
    { coords : ( Int, Int ), liveness : Liveness }


type alias GameGrid =
    Array2D GameCell


type alias Model =
    { grid : GameGrid
    , key : Nav.Key
    , url : Url.Url
    , pressedKeys : List Kb.Key
    , timeDelta : Float
    , tick : Int
    , pauseState : PauseState
    }


type Msg
    = ToggleCell ( Int, Int )
    | Link Browser.UrlRequest
    | NewUrl Url.Url
    | KeyMsg Kb.Msg
    | TimeDelta Float
    | TogglePause
    | Reset


initGrid : GameGrid
initGrid =
    Array2D.initialize 30 30 (\_ _ -> Dead)
        |> Array2D.set 0 0 Alive
        |> Array2D.set 1 1 Alive
        |> Array2D.set 1 2 Alive
        |> Array2D.set 2 0 Alive
        |> Array2D.set 2 1 Alive
        |> Array2D.set 4 4 Alive
        |> Array2D.set 5 5 Alive
        |> Array2D.set 5 6 Alive
        |> Array2D.set 6 4 Alive
        |> Array2D.set 6 5 Alive
        |> Array2D.set 8 8 Alive
        |> Array2D.set 9 9 Alive
        |> Array2D.set 9 10 Alive
        |> Array2D.set 10 8 Alive
        |> Array2D.set 10 9 Alive
        --
        |> Array2D.set (29 - 0) 0 Alive
        |> Array2D.set (29 - 1) 1 Alive
        |> Array2D.set (29 - 2) 1 Alive
        |> Array2D.set (29 - 0) 2 Alive
        |> Array2D.set (29 - 1) 2 Alive
        |> Array2D.set (29 - 4) 4 Alive
        |> Array2D.set (29 - 5) 5 Alive
        |> Array2D.set (29 - 6) 5 Alive
        |> Array2D.set (29 - 4) 6 Alive
        |> Array2D.set (29 - 5) 6 Alive
        |> Array2D.set (29 - 8) 8 Alive
        |> Array2D.set (29 - 9) 9 Alive
        |> Array2D.set (29 - 10) 9 Alive
        |> Array2D.set (29 - 8) 10 Alive
        |> Array2D.set (29 - 9) 10 Alive
        --
        |> Array2D.set 0 (29 - 0) Alive
        |> Array2D.set 1 (29 - 1) Alive
        |> Array2D.set 2 (29 - 1) Alive
        |> Array2D.set 0 (29 - 2) Alive
        |> Array2D.set 1 (29 - 2) Alive
        |> Array2D.set 4 (29 - 4) Alive
        |> Array2D.set 5 (29 - 5) Alive
        |> Array2D.set 6 (29 - 5) Alive
        |> Array2D.set 4 (29 - 6) Alive
        |> Array2D.set 5 (29 - 6) Alive
        |> Array2D.set 8 (29 - 8) Alive
        |> Array2D.set 9 (29 - 9) Alive
        |> Array2D.set 10 (29 - 9) Alive
        |> Array2D.set 8 (29 - 10) Alive
        |> Array2D.set 9 (29 - 10) Alive
        --
        |> Array2D.set (29 - 0) (29 - 0) Alive
        |> Array2D.set (29 - 1) (29 - 1) Alive
        |> Array2D.set (29 - 1) (29 - 2) Alive
        |> Array2D.set (29 - 2) (29 - 0) Alive
        |> Array2D.set (29 - 2) (29 - 1) Alive
        |> Array2D.set (29 - 4) (29 - 4) Alive
        |> Array2D.set (29 - 5) (29 - 5) Alive
        |> Array2D.set (29 - 5) (29 - 6) Alive
        |> Array2D.set (29 - 6) (29 - 4) Alive
        |> Array2D.set (29 - 6) (29 - 5) Alive
        |> Array2D.set (29 - 8) (29 - 8) Alive
        |> Array2D.set (29 - 9) (29 - 9) Alive
        |> Array2D.set (29 - 9) (29 - 10) Alive
        |> Array2D.set (29 - 10) (29 - 8) Alive
        |> Array2D.set (29 - 10) (29 - 9) Alive
        --}
        |> Array2D.indexedMap
            (\x y liveness -> { coords = ( x, y ), liveness = liveness })


{-| FIXME: Define the structure of a pulsar based on initial x and y coords.

      ---------------
      ---###---###---
      ---------------
      -#----#-#----#-
      -#----#-#----#-
      -#----#-#----#-
      ---###---###---
      ---------------
      ---###---###---
      -#----#-#----#-
      -#----#-#----#-
      -#----#-#----#-
      ---------------
      ---###---###---
      ---------------

-}



--pulsar : Int -> Int -> GameGrid -> GameGrid


pulsar x y grid =
    let
        hBars j =
            [ ( x + 3, j + 1 )
            , ( x + 4, j + 1 )
            , ( x + 5, j + 1 )
            , ( x + 9, j + 1 )
            , ( x + 10, j + 1 )
            , ( x + 11, j + 1 )
            ]

        vCrossSection j =
            [ ( x + 2, j )
            , ( x + 7, j )
            , ( x + 9, j )
            , ( x + 14, j )
            ]

        vBars j =
            vCrossSection 0
                ++ vCrossSection 1
                ++ vCrossSection 2

        pulsarDef =
            hBars 1
                ++ vBars 3
                ++ hBars 6
                ++ hBars 8
                ++ vBars 9
                ++ hBars 13
    in
    List.map (\( i, j ) -> { coords = ( i, j ), liveness = Alive }) pulsarDef


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model
        initGrid
        key
        url
        []
        0
        0
        Paused
    , Cmd.none
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCell ( x, y ) ->
            ( case Array2D.get x y model.grid of
                Just cell ->
                    { model
                        | grid = Array2D.set x y (toggleCell cell) model.grid
                    }

                Nothing ->
                    model
            , Cmd.none
            )

        Link _ ->
            ( model, Cmd.none )

        NewUrl _ ->
            ( model, Cmd.none )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Kb.update keyMsg model.pressedKeys }
            , Cmd.none
            )

        TimeDelta delta ->
            if model.timeDelta + delta >= 1 then
                ( { model
                    | grid = updateGameState model.grid
                    , timeDelta = 0
                    , tick = model.tick + 1
                  }
                , Cmd.none
                )
            else
                ( { model | timeDelta = model.timeDelta + delta }
                , Cmd.none
                )

        TogglePause ->
            ( { model
                | pauseState =
                    case model.pauseState of
                        Running ->
                            Paused

                        Paused ->
                            Running
              }
            , Cmd.none
            )

        Reset ->
            ( { model | grid = initGrid }
            , Cmd.none
            )


updateGameState : GameGrid -> GameGrid
updateGameState grid =
    Array2D.indexedMap (updateCell grid) grid


updateCell : GameGrid -> Int -> Int -> GameCell -> GameCell
updateCell grid x y cell =
    let
        neighborList =
            [ ( x - 1, y - 1 )
            , ( x, y - 1 )
            , ( x + 1, y - 1 )
            , ( x - 1, y )
            , ( x + 1, y )
            , ( x - 1, y + 1 )
            , ( x, y + 1 )
            , ( x + 1, y + 1 )
            ]

        getCell ( xPos, yPos ) =
            Array2D.get xPos yPos grid
                |> Maybe.withDefault (GameCell ( xPos, yPos ) Dead)

        neighborSum =
            List.map getCell neighborList
                |> List.foldl cellToCount 0
    in
    GameCell ( x, y ) <|
        case cell.liveness of
            Alive ->
                if neighborSum < 2 then
                    Dead
                else if neighborSum > 3 then
                    Dead
                else
                    Alive

            Dead ->
                if neighborSum == 3 then
                    Alive
                else
                    Dead


cellToCount : GameCell -> Int -> Int
cellToCount cell acc =
    case cell.liveness of
        Alive ->
            acc + 1

        Dead ->
            acc


toggleCell : GameCell -> GameCell
toggleCell cell =
    case cell.liveness of
        Alive ->
            { cell | liveness = Dead }

        Dead ->
            { cell | liveness = Alive }



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    case model.pauseState of
        Paused ->
            Sub.none

        Running ->
            Sub.batch
                [ Sub.map KeyMsg Kb.subscriptions
                , Events.onAnimationFrameDelta TimeDelta
                ]



-- View --


view : Model -> Browser.Document Msg
view model =
    { title = "Life!"
    , body =
        [ Element.layout
            [ width fill
            , height fill
            , Font.family [ Font.monospace ]
            , Font.color <| inv <| rgb 1 1 1
            , Bg.color <| inv <| rgb 0 0 0
            ]
          <|
            column [ centerX, centerY ] <|
                (::) (topBar model) <|
                    List.map
                        (livenessGridToEl model.grid)
                    <|
                        List.range 0 (Array2D.rows model.grid - 1)
        ]
    }


topBar : Model -> Element Msg
topBar model =
    row [ spacing 10 ]
        [ Input.button
            [ padding 5
            , width <| px 100
            , Font.center
            , Border.width 1
            , Bg.color <| inv <| rgb 0.5 0 0
            ]
            { onPress = Just Reset
            , label = text "Reset"
            }
        , Input.button
            [ padding 5
            , width <| px 100
            , Font.center
            , Border.width 1
            , Bg.color <| inv <| rgb 0 0.5 0.75
            ]
            { onPress = Just TogglePause
            , label =
                text <|
                    case model.pauseState of
                        Paused ->
                            "Unpause"

                        Running ->
                            "Pause"
            }
        , el [ padding 5 ] <| text <| String.fromInt model.tick
        ]


livenessGridToEl : GameGrid -> Int -> Element Msg
livenessGridToEl grid =
    let
        elGrid =
            grid
                |> Array2D.map livenessToEl

        toElRow n =
            row [] <|
                (Array2D.getRow n elGrid
                    |> Maybe.map Array.toList
                    |> Maybe.withDefault []
                )
    in
    toElRow


livenessToEl : GameCell -> Element Msg
livenessToEl cell =
    let
        side =
            20

        style =
            [ width <| px side
            , height <| px side
            , Border.width 1
            , Border.color <| inv <| rgb 0.5 0.5 0.5
            , Font.size 10
            , onClick <| ToggleCell cell.coords
            ]

        cellTextEl =
            el [ centerX, centerY ] <|
                none

        {- text <|
           String.fromInt (Tuple.first cell.coords)
               ++ "-"
               ++ String.fromInt (Tuple.second cell.coords)
        -}
    in
    case cell.liveness of
        Alive ->
            el
                ((Bg.color <| inv <| rgb 0.5 1 0)
                    :: Font.bold
                    :: style
                )
            <|
                cellTextEl

        Dead ->
            el
                ((Bg.color <| inv <| rgb 0.1 0.1 0.1)
                    :: style
                )
            <|
                cellTextEl


isInverted : Bool
isInverted =
    --False
    True


inv : Color -> Color
inv color =
    case isInverted of
        True ->
            let
                rgb =
                    color |> toRgb
            in
            { rgb
                | red = 1.0 - rgb.red
                , green = 1.0 - rgb.green
                , blue = 1.0 - rgb.blue
            }
                |> fromRgb

        False ->
            color


noBorder =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }
