import Html exposing (..)
import Browser

main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }


-- MODEL

type alias Model = ()

init : () -> (Model, Cmd Msg)
init _ = ((), Cmd.none)


-- UPDATE

type alias Msg = ()

update : Msg -> Model -> (Model, Cmd Msg)
update _ _ = ((), Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- VIEW

view : Model -> Html Msg
view model = text "Lorem ipsum"