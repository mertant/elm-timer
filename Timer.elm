import Html exposing (Html, div, h1, text, button)
import Html.Attributes exposing (style)
import Html.App as Html
import Html.Events exposing (onClick)
import Time exposing (..)

main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL --

type alias Model =
  { start : Time
  , intervalType: IntervalType
  , timer: Time
  , stopped: Bool
  }

init : (Model, Cmd Msg)
init =
  (Model 0 Tomato (duration Tomato) True,  Cmd.none)


type IntervalType = Tomato | ShortBreak | LongBreak

duration : IntervalType -> Time
duration intervalType =
  case intervalType of
    Tomato -> 25*60
    ShortBreak -> 5--*60
    LongBreak -> 15*60



-- UPDATE --

type Msg
  = Tick Time
  | ToggleStop
  | Reset IntervalType

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick newTime ->
      (tick model newTime, Cmd.none)
    ToggleStop ->
      (toggleStop model, Cmd.none)
    Reset intervalType ->
      (resetTimer model intervalType, Cmd.none)

tick : Model -> Time -> Model
tick model newTime =
  Model
  (if (model.start == 0) then newTime else model.start)
  model.intervalType
  (if (model.stopped) then model.timer else (model.timer - 1))
  model.stopped

toggleStop: Model -> Model
toggleStop model =
  { model
  | stopped = not model.stopped
  }

resetTimer: Model -> IntervalType -> Model
resetTimer model intervalType =
  { model
  | start = 0
  , stopped = True
  , intervalType = intervalType
  , timer = (duration intervalType)
  }


millisToSeconds : Float -> Int
millisToSeconds millis =
  floor(millis/1000)

formatTime : Float -> String
formatTime seconds =
  let
    minutes = floor (seconds / 60)
    mm = (if (minutes < 10) then "0" else "") ++ toString minutes
    secondRem = round seconds % 60
    ss = (if (secondRem < 10) then "0" else "") ++ toString secondRem
  in
  mm ++ ":" ++ ss



-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick



-- VIEW --

view : Model -> Html Msg
view model =
  div [ style [ ("text-align", "center"), ("margin", "5rem"), ("max-width", "40rem")] ]
  [ h1 [] [ text (toString model.intervalType) ]
  , div [ flexRow ]
    [ buttonView (Reset Tomato) "Tomato (work)"
    , buttonView (Reset ShortBreak) "Short break (DEBUG)"
    , buttonView (Reset LongBreak) "Long break"
    ]
  , (timerView model)
  , div [ flexRow ]
    [ buttonView ToggleStop "Stop/Start"
    , buttonView (Reset model.intervalType) "Reset"
    ]
  ]

buttonView : Msg -> String -> Html Msg
buttonView event string =
  button [ buttonStyle, onClick event ] [ text string ]

timerView : Model -> Html Msg
timerView model =
  div [ timerStyle ] [ text  ( formatTime model.timer) ]



-- STYLES --

flexRow =
  style [ ("flex-direction", "row"), ("display", "flex")]

buttonStyle =
  style [ ("flex", "1"), ("font-size", "2rem") ]

timerStyle =
  style [ ("font-size", "8rem"), ("font-family", "sans-serif") ]
