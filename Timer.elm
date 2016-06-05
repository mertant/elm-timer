import Html exposing (Html, div, h2, text, button)
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



-- MODEL

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



-- UPDATE

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

{--elapsedSeconds : Model -> Int
elapsedSeconds model =
  (millisToSeconds(Time.tick - model.start))--}

formatTime : Float -> String
formatTime seconds =
  let
    minutes = floor (seconds / 60)
    mm = (if (minutes < 10) then "0" else "") ++ toString minutes
    secondRem = round seconds % 60
    ss = (if (secondRem < 10) then "0" else "") ++ toString secondRem
  in
  mm ++ ":" ++ ss



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick



-- VIEW

view : Model -> Html Msg
view model =
  div []
  [ h2 [] [ text (toString model.intervalType) ]
  , button [ onClick (Reset Tomato) ] [text "Tomato (work)"]
  , button [ onClick (Reset ShortBreak) ] [text "Short break"]
  , button [ onClick (Reset LongBreak) ] [text "Long break"]
  --, div [] [] , text ( "Started at ") , text (formatTime (model.start))
  , div [] []
  , text ( "Countdown " )
  , text ( formatTime model.timer)
  , button [ onClick ToggleStop ] [ text "Stop/Start" ]
  , button [ onClick (Reset model.intervalType) ] [ text "Reset" ]
  ]
