import Html
import Browser
import Array

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
  { hraci : Array.Array Hrac
  , hrac : Int
  , faza : Faza
  , sirka : Int
  , vyska : Int
  }

type alias Hrac =
  { zivot : Int
  , sur : Maybe Poloha
  , kamen : Int
  , sipkaS : Bool
  , sipkaV : Bool
  , sipkaJ : Bool
  , sipkaZ : Bool
  , mina : Int
  , veza : Bool
  , zrkadloV : Bool
  , zrkadloZ : Bool
  , radar : Bool
  , sonar : Bool
  , laser : Bool
  , tank : Bool
  }

type Faza
  = Podanie
  | Tah
  | Zaver

init : () -> (Model, Cmd Msg)
init _ = (
  { hraci = Array.empty
  , hrac = 0
  , faza = Podanie
  , sirka = 10 -- zatiaľ napevno
  , vyska = 10
  }, Cmd.none)

novyhrac : Hrac
novyhrac =
  { zivot = 3
  , sur = Nothing
  , kamen = 10
  , sipkaS = True
  , sipkaV = True
  , sipkaJ = True
  , sipkaZ = True
  , mina = 4
  , veza = False
  , zrkadloV = False
  , zrkadloZ = False
  , radar = False
  , sonar = False
  , laser = False
  , tank = False
  }

-- Behanie po mape

type alias Poloha = { x : Int, y : Int}

type alias Smer = (Poloha -> Poloha)

sever : Model -> Smer
sever {vyska} p = { p | y = modBy vyska (p.y - 1) }

vychod : Model -> Smer
vychod {sirka} p = { p | x = modBy sirka (p.x + 1) }

juh : Model -> Smer
juh {vyska} p = { p | y = modBy vyska (p.y + 1) }

zapad : Model -> Smer
zapad {sirka} p = { p | x = modBy sirka (p.x - 1) }

type alias Odraz = (Smer -> Smer)

odrazV : Model -> Odraz
odrazV m s p = { x = modBy m.sirka (p.x - (s p).y + p.y), y = modBy m.vyska (p.y - (s p).x + p.x) }

odrazZ : Model -> Odraz
odrazZ m s p = { x = modBy m.sirka (p.x + (s p).y - p.y), y = modBy m.vyska (p.y + (s p).x - p.x) }


-- UPDATE

type Msg
  = Prijal
  | PrijalNovy
  | PrijalNula
  | Podal
  | UmiestnilSa Poloha

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({hrac, hraci} as model) = (
  let
    hracNaTahu = Maybe.withDefault novyhrac (Array.get hrac hraci)
    tahaj m = { m | faza = Zaver }
  in
    case msg of
      Prijal -> { model | faza = Tah }
      PrijalNovy -> { model | hraci = Array.push novyhrac hraci, faza = Tah }
      PrijalNula -> { model | hrac = 0, faza = Tah }
      Podal -> { model | hrac = if Array.isEmpty hraci then hrac + 1 else modBy (Array.length hraci) (hrac + 1), faza = Podanie }
      UmiestnilSa p -> tahaj { model | hraci = Array.set hrac { hracNaTahu | sur = Just p } hraci }
  , Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- VIEW

view : Model -> Html.Html Msg
view {hraci, hrac, faza, sirka, vyska} = (
  let
    tlacidlo farba msg =
      [ onClick msg
      , El.width El.fill
      , El.height El.fill
      , El.padding 8
      , Font.color (El.rgb 1 1 1)
      , Bg.color farba
      , El.pointer
      ]
  in
    case faza of
      Podanie ->
        if hrac >= Array.length hraci then -- počítanie hráčov na začiatku hry
          El.column [ El.width El.fill, El.height El.fill ]
            [ El.text ("Som hráč " ++ String.fromInt hrac) |> El.el (tlacidlo (El.rgb 0 0.6 1) PrijalNovy)
            , if hrac > 0 then
                El.text ("Som hráč 0") |> El.el (tlacidlo (El.rgb 1 0.8 0) PrijalNula)
              else
                El.none
            ]
        else -- nový hráč prichádza na ťah
          El.text ("Som hráč " ++ String.fromInt hrac) |> El.el (tlacidlo (El.rgb 1 0.6 1) Prijal)
      Tah ->
        case Array.get hrac hraci of
          Nothing -> -- TODO: chybová stránka (alebo zmena modelu, aby sme nemuseli Array.get)
            El.none
          Just h ->
            case h.sur of
              Nothing -> -- umiestni sa na mape
                List.range 0 (vyska - 1)
                |> List.map
                    (\y ->
                      List.range 0 (sirka - 1)
                      |> List.map (\x -> El.el (tlacidlo (El.rgb 0 0 0) (UmiestnilSa (Poloha x y))) El.none)
                      |> El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                    )
                |> El.column [ El.width El.fill, El.height El.fill, El.spacing 8, El.padding 8 ]
              Just {x, y} -> -- TODO: normálne ťahy
                El.none
      Zaver -> -- TODO: čo hráč vidí po svojom ťahu
        El.none
  ) |> El.layout [ Font.center ]