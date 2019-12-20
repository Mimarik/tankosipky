module Main exposing (main)

import Array
import Browser
import Element as El
import Element.Background as Bg
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Html


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  { faza : Faza
  , log : List Tah
  }


type alias Stav =
  { hraci : Array.Array Hrac
  , hrac : Int
  , hracov : Int
  , sirka : Int
  , vyska : Int
  , mapa : Array.Array (Array.Array Policko)
  }


type Policko
  = Nic
  | Tank Smer
  | Gula Smer
  | Mina
  | MinaGula Smer
  | Kamen
  | Laser Smer
  | Veza Smer
  | Clovek Int
  | Zrkadlo Odraz
  | Luc Smer
  | MinaLuc Smer
  | Sipka Smer


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
  | Dumanie
  | Zaver
  | Konfiguracia Int Int Int


init : () -> ( Model, Cmd Msg )
init _ =
  ( { faza = Konfiguracia 4 10 10, log = [] }, Cmd.none )


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


novystav : Stav
novystav =
  { hraci = Array.empty
  , hrac = 0
  , hracov = 0
  , sirka = 0
  , vyska = 0
  , mapa = Array.empty
  }



-- BEHANIE PO MAPE


type alias Poloha =
  { x : Int, y : Int }


type alias Smer =
  Poloha -> Poloha


smer : Stav -> Int -> Int -> Smer
smer { sirka, vyska } dx dy { x, y } =
  { x = modBy sirka (x + dx), y = modBy vyska (y + dy) }


sever : Stav -> Smer
sever m =
  smer m 0 -1


vychod : Stav -> Smer
vychod m =
  smer m 1 0


juh : Stav -> Smer
juh m =
  smer m 0 1


zapad : Stav -> Smer
zapad m =
  smer m -1 0


type alias Odraz =
  Smer -> Smer


odrazV : Stav -> Odraz
odrazV m s p =
  { x = modBy m.sirka (p.x - (s p).y + p.y), y = modBy m.vyska (p.y - (s p).x + p.x) }


odrazZ : Stav -> Odraz
odrazZ m s p =
  { x = modBy m.sirka (p.x + (s p).y - p.y), y = modBy m.vyska (p.y + (s p).x - p.x) }



-- UPDATE


type Msg
  = Prijal
  | Podal
  | Tahal Tah
  | Konfiguroval Int Int Int


type Tah
  = Zmapuj Int Int
  | Zrod Int
  | UmiestniSa Poloha


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( case msg of
    Prijal ->
      { model | faza = Dumanie }
    Tahal t ->
      { model | log = model.log ++ [ t ], faza = if model.faza == Dumanie then Zaver else model.faza }
    Podal ->
      { model | faza = Podanie }
    Konfiguroval n s v ->
      { model | faza = Konfiguracia n s v }
  , Cmd.none
  )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
  (let
    tlacidlo farba msg obsah =
      Input.button
        [ El.width El.fill
        , El.height El.fill
        , El.padding 8
        , Font.color (El.rgb 1 1 1)
        , Bg.color farba
        , El.pointer
        , El.focused [ Border.color (El.rgba 0 0 0 0) ]
        ]
        { onPress = Just msg
        , label = obsah
        }
    stav =
      List.foldl vykonaj novystav model.log
    vykonaj t s =
      let
        hracNT =
         s.hraci |> Array.get s.hrac |> Maybe.withDefault novyhrac
      in
        case t of
          Zmapuj sirka vyska ->
            { s | sirka = sirka, vyska = vyska, mapa = Array.repeat sirka (Array.repeat vyska Nic) }
          Zrod n ->
            { s | hracov = n, hraci = Array.repeat n novyhrac }
          UmiestniSa p ->
            dalsi { s | hraci = Array.set s.hrac { hracNT | sur = Just p } s.hraci }
    dalsi s =
      { s | hrac = modBy s.hracov (s.hrac + 1) }
    aktivny =
      if model.faza == Zaver then
        modBy stav.hracov (stav.hrac - 1)
      else
        stav.hrac
    najdi { x, y } =
      stav.mapa |> Array.get x |> Maybe.withDefault Array.empty |> Array.get y |> Maybe.withDefault Nic
    ukaz obj =
      if obj == Nic then
        El.text "Nič"
      else
        El.text "Niečo"
      |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.8 0.8 0.8) ]
    hracNaTahu =
      stav.hraci |> Array.get aktivny |> Maybe.withDefault novyhrac
  in
    case model.faza of
      Konfiguracia n s v ->
        if stav.hracov == 0 then
          -- voľba počtu hráčov
          El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
            [ tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval (n + 1) s v) (El.text "Zvýšiť počet hráčov")
            , tlacidlo (El.rgb 0.1 0.1 0.1) (Zrod n |> Tahal) (El.text (String.fromInt n))
            , tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval (max 1 (n - 1)) s v) (El.text "Znížiť počet hráčov")
            ]
        else if stav.sirka == 0 then
          -- voľba rozmerov mapy
          El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
            [ El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
              [ tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n (s + 1) v) (El.text "Zvýšiť šírku mapy")
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Zmapuj s v |> Tahal) (El.text (String.fromInt s))
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n (max 1 (s - 1)) v) (El.text "Znížiť šírku mapy")
              ]
            , El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
              [ tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n s (v + 1)) (El.text "Zvýšiť výšku mapy")
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Zmapuj s v |> Tahal) (El.text (String.fromInt v))
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n s (max 1 (v - 1))) (El.text "Znížiť výšku mapy")
              ]
            ]
        else
          -- nový hráč prichádza na ťah
          El.text ("Som hráč " ++ String.fromInt aktivny) |> tlacidlo (El.rgb 0 0.6 1) Prijal
      Podanie ->
        -- nový hráč prichádza na ťah
        El.text ("Som hráč " ++ String.fromInt aktivny) |> tlacidlo (El.rgb 0 0.6 1) Prijal
      Dumanie ->
        case hracNaTahu.sur of
          Nothing ->
            -- umiestni sa na mape
            List.range 0 (stav.vyska - 1)
              |> List.map
                (\y ->
                  List.range 0 (stav.sirka - 1)
                    |> List.map (\x -> tlacidlo (El.rgb 0.8 0.8 0.8) (Poloha x y |> UmiestniSa |> Tahal) El.none)
                    |> El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                )
              |> El.column [ El.width El.fill, El.height El.fill, El.spacing 8, El.padding 8 ]
          Just { x, y } ->
            -- TODO: normálne ťahy
            El.none
      Zaver ->
        case hracNaTahu.sur of
          Nothing ->
            -- TODO: hráč medzičasom opäť zomrel
            El.none
          Just p ->
            -- čo hráč vidí po svojom ťahu
            El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
              [ El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                [ El.el [ El.width El.fill ] El.none
                , sever stav p |> najdi |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
                , El.el [ El.width El.fill ] El.none
                ]
              , El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                [ zapad stav p |> najdi |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
                , p |> najdi |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
                , vychod stav p |> najdi |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
                ]
              , El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                [ El.el [ El.width El.fill ] El.none
                , juh stav p |> najdi |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
                , El.el [ El.width El.fill ] El.none
                ]
              , El.text ("Podávam hráčovi " ++ String.fromInt stav.hrac) |> tlacidlo (El.rgb 0.4 0.8 0) Podal
              ]
  )
    |> El.layout [ Font.center, Bg.color (El.rgb 0 0 0) ]
