import Html exposing (..)
import Browser
import String exposing (fromInt)

import Element as El
import Element.Border as Border
import Element.Background as Bg
import Element.Font as Font
import Element.Events exposing (onClick)

main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }


-- MODEL

type alias Model =
  { hracov : Maybe Int
  , hrac : Int
  , prijate : Bool
  }

init : () -> (Model, Cmd Msg)
init _ = (
  { hracov = Nothing
  , hrac = 1
  , prijate = False
  }, Cmd.none)


-- UPDATE

type Msg
  = Prijal
  | Vsetci

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({hrac, hracov} as model) = (
  case msg of
    Prijal ->
      case hracov of
        Nothing -> { model | hrac = hrac + 1 }
        Just n -> { model | hrac = modBy n (hrac + 1) }
    Vsetci -> { model | hrac = 0, hracov = Just (hrac + 1) }
  , Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- VIEW

view : Model -> Html Msg
view {hracov, hrac, prijate} = El.layout [ Font.center ] <|
  if prijate then
    El.none
  else
    case hracov of
      Nothing -> El.column [ El.width El.fill, El.height El.fill ]
        [ El.el (potvrd Prijal) <| El.text ("Som hráč " ++ fromInt hrac)
        , El.el [ onClick Vsetci ] <|  El.text ("Som hráč 0")
        ]
      Just _ -> El.el (potvrd Prijal) <| El.text ("Som hráč " ++ fromInt hrac)

potvrd : msg -> List (El.Attribute msg)
potvrd msg =
  [ onClick msg
  , El.width El.fill
  , El.height El.fill
  , El.padding 8
  , Bg.color (El.rgb 0 0.6 1)
  , Font.color (El.rgb 1 1 1)
  , El.pointer
  ]