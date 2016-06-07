
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
  , stage: Stage
  , timer: Time
  , isStopped: Bool
  , timerReachedEnd: Bool
  }

init : (Model, Cmd Msg)
init =
  (Model 0 Tomato (duration Tomato) True False,  Cmd.none)


type Stage
  = Tomato
  | ShortBreak
  | LongBreak

duration : Stage -> Time
duration stage =
  case stage of
    Tomato -> 25*60
    ShortBreak -> 5--*60
    LongBreak -> 15*60




-- UPDATE --

type Msg
  = Tick Time
  | ToggleStop
  | Reset Stage

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick newTime ->
      (tick model newTime, Cmd.none)

    ToggleStop ->
      (toggleStop model, Cmd.none)

    Reset stage ->
      (resetTimer model stage, Cmd.none)


tick : Model -> Time -> Model
tick model newTime =
  let
    start = if (model.start == 0) then newTime else model.start
    stage = model.stage
    isStopped = model.isStopped || timer <= 0
    timer = if (isStopped || model.timer <= 0) then model.timer else (model.timer - 1)
    timerReachedEnd = (not model.isStopped && timer <= 0)
  in
    Model start stage timer isStopped timerReachedEnd

toggleStop: Model -> Model
toggleStop model =
  { model
  | isStopped = not model.isStopped
  , timerReachedEnd = False
  }

resetTimer: Model -> Stage -> Model
resetTimer model stage =
  { model
  | start = 0
  , isStopped = True
  , stage = stage
  , timer = (duration stage)
  , timerReachedEnd = False
  }


formatTime : Time -> String
formatTime seconds =
  let
    absSeconds = abs seconds
    minutes = floor (absSeconds / 60)
    mm = (if (minutes < 10) then "0" else "") ++ toString minutes
    secondRem = round absSeconds % 60
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
  let
    title =
      "Task Timer"
    topButtonRow =
      div [ flexRow ]
        [ setIntervalTypeButton model Tomato "Tomato (work)"
        , setIntervalTypeButton model ShortBreak "Short break (DEBUG)"
        , setIntervalTypeButton model LongBreak "Long break"
        ]
    bottomButtonRow =
      div [ flexRow ]
        [ buttonView (model.isStopped) ToggleStop (if model.isStopped then "Start" else "Stop")
        , buttonView False (Reset model.stage) "Reset"
        ]
    alarmRow =
      div [ alarmStyle model.timerReachedEnd ]
      [ text (if (model.timerReachedEnd)
        then "BEEP BEEP BEEP BEEP BEEP BEEP (press any button to stop alarm)"
        else "Note: there is no audible alarm.")
      ]
  in
    div [ mainStyle ]
    [ h1 [ titleStyle ] [ text title ]
    , topButtonRow
    , (timerView model)
    , bottomButtonRow
    , alarmRow
    ]


setIntervalTypeButton : Model -> Stage -> String -> Html Msg
setIntervalTypeButton model stage string =
  let
    isSelected = (stage == model.stage)
    event = (Reset stage)
  in
    button [ buttonStyle isSelected, onClick event ] [ text string ]

buttonView : Bool -> Msg -> String -> Html Msg
buttonView isSelected event string =
  button [ buttonStyle isSelected, onClick event ] [ text string ]

timerView : Model -> Html Msg
timerView model =
  div [ timerStyle model.isStopped ] [ text  ( formatTime model.timer) ]



-- STYLES --

light: String
light = "#1598b2"
dark: String
dark = "#001330"
orangeish: String
orangeish = "#ff6c11"

mainStyle : Html.Attribute a
mainStyle =
  style [ ("text-align", "center")
        , ("margin", "5rem")
        , ("max-width", "40rem")
        , ("font-family", "sans-serif")
        ]

titleStyle: Html.Attribute a
titleStyle =
   style [ ("font-size", "5rem")
         , ("color", orangeish)
         ]

flexRow : Html.Attribute a
flexRow =
  style [ ("flex-direction", "row")
        , ("display", "flex") ]

buttonStyle : Bool -> Html.Attribute a
buttonStyle isSelected =
  style [ ("flex", "1")
        , ("font-size", "2rem")
        , ("color", "white")
        , ("background-color", (if (isSelected) then light else dark)) ]

timerStyle : Bool -> Html.Attribute a
timerStyle isStopped = style
      [ ("font-size", "8rem")
      , ("color", if (isStopped) then orangeish else light)
      ]

alarmStyle : Bool -> Html.Attribute a
alarmStyle isActive = style
  [ ("font-size", (if isActive then "6rem" else "2rem"))
  , ("color", if (isActive) then orangeish else light)
  ]
