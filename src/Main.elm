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
  | Zrkadlo Odraz Bool Bool
  | Luc Smer
  | MinaLuc Smer
  | Sipka Smer


type alias Hrac =
  { zivot : Int
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
  | Dumanie Policko
  | Zaver
  | Konfiguracia Int Int Int


init : () -> ( Model, Cmd Msg )
init _ =
  ( { faza = Konfiguracia 4 10 10, log = [] }, Cmd.none )


novyHrac : Hrac
novyHrac =
  { zivot = 3
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


novyStav : Stav
novyStav =
  { hraci = Array.empty
  , hrac = 0
  , hracov = 0
  , sirka = 0
  , vyska = 0
  , mapa = Array.empty
  }


polohaHraca : Stav -> Int -> Maybe Poloha
polohaHraca s n =
  s.mapa
    |> Array.map (Array.toIndexedList >> List.foldl (\(y, e) a -> if e == Clovek n then Just y else a) Nothing)
    |> Array.toIndexedList
    |> List.foldl (\(x, e) a ->
      case e of
        Just y ->
          Just { x = x, y = y }
        Nothing ->
          a
    ) Nothing


policko : Array.Array (Array.Array Policko) -> Poloha -> Policko
policko mapa { x, y } =
  mapa |> Array.get x |> Maybe.withDefault Array.empty |> Array.get y |> Maybe.withDefault Nic


nastav : Poloha -> Policko -> Array.Array (Array.Array Policko) -> Array.Array (Array.Array Policko)
nastav p o mapa =
  Array.set p.x (Array.get p.x mapa |> Maybe.withDefault Array.empty |> Array.set p.y o) mapa


vykonaj : Tah -> Stav -> Stav
vykonaj t s =
  let
    poloz obj p pokus mapa =
      let
        staryobj =
          policko mapa p
      in
        if pokus > s.sirka * s.vyska then
          mapa
        else
          case (obj, staryobj) of
              (Nic, _) ->
                mapa
              (_, Sipka kam) ->
                mapa |> poloz obj (p |> smer s kam) (pokus + 1)
              (Sipka kam, _) ->
                mapa |> nastav p obj |> poloz staryobj (p |> smer s kam) 0
              (_, Tank _) ->
                mapa
              (Tank _, _) ->
                mapa |> nastav p obj
              (Mina, Gula g) ->
                mapa |> nastav p (MinaGula g)
              (MinaGula _, Gula g) ->
                mapa |> nastav p  (MinaGula g)
              (MinaLuc _, Gula g) ->
                mapa |> nastav p (MinaGula g)
              (_, Gula _) ->
                mapa
              (Mina, MinaGula g) ->
                mapa |> nastav p (Gula g)
              (MinaGula _, MinaGula g) ->
                mapa |> nastav p (Gula g)
              (MinaLuc _, MinaGula g) ->
                mapa |> nastav p (Gula g)
              (_, MinaGula _) ->
                mapa
              (Gula g, Mina) ->
                mapa |> nastav p (MinaGula g)
              (Gula _, _) ->
                mapa |> nastav p obj
              (MinaGula g, Mina) ->
                mapa |> nastav p (Gula g)
              (MinaGula _, _) ->
                mapa |> nastav p obj 
              (Luc l, Mina) ->
                mapa |> nastav p (MinaLuc l)
              (_, Mina) ->
                mapa |> nastav p Nic
              (Luc _, MinaLuc _) ->
                mapa
              (_, MinaLuc l) ->
                mapa |> nastav p (Luc l)
              (Mina, Nic) ->
                mapa |> nastav p obj
              (Mina, Luc l) ->
                mapa |> nastav p (MinaLuc l)
              (Mina, _) ->
                mapa |> nastav p Nic
              (MinaLuc _, Nic) ->
                mapa |> nastav p obj
              (MinaLuc _, Luc _) ->
                mapa |> nastav p obj
              (MinaLuc l, _) ->
                mapa |> nastav p (Luc l)
              (_, Kamen) ->
                mapa
              (Kamen, _) ->
                mapa |> nastav p obj
              (_, Laser _) ->
                mapa
              (Laser _, _) ->
                mapa |> nastav p obj
              (_, Veza _) ->
                mapa
              (Veza _, _) ->
                mapa |> nastav p obj
              (Zrkadlo _ _ _, Luc _) ->
                mapa |> nastav p obj
              (_, Luc _) ->
                mapa
              (Luc _, Zrkadlo _ _ _) ->
                mapa
              (Luc _, _) ->
                mapa |> nastav p obj
              (_, Clovek _) ->
                mapa
              (Clovek _, _) ->
                mapa |> nastav p obj
              (Zrkadlo _ _ _, Zrkadlo _ _ _) ->
                mapa
              (Zrkadlo _ _ _, Nic) ->
                mapa |> nastav p obj
    sur =
      polohaHraca s s.hrac |> Maybe.withDefault (Poloha 0 0)
    vyprazdni p mapa =
      Array.set p.x
        (Array.get p.x mapa |> Maybe.withDefault Array.empty |> Array.set p.y
          (case mapa |> Array.get p.x |> Maybe.withDefault Array.empty |> Array.get p.y |> Maybe.withDefault Nic of
            Nic ->
              Nic
            Tank _ ->
              Nic
            Gula _ ->
              Nic
            MinaGula _ ->
              Mina
            MinaLuc _ ->
              Mina
            Luc _ ->
              Nic
            Clovek _ ->
              Nic
            Zrkadlo z _ _ ->
              Zrkadlo z False False
            x ->
              x
          )
        ) mapa
  in
    case t of
      Zmapuj sirka vyska ->
        { s | sirka = sirka, vyska = vyska, mapa = Array.repeat sirka (Array.repeat vyska Nic) }
      Zrod n ->
        { s | hracov = n, hraci = Array.repeat n novyHrac }
      UmiestniSa p ->
        { s | mapa = poloz (Clovek s.hrac) p 0 s.mapa } |> dalsi
      Chod kam ->
        let
          p =
            smer s kam sur
        in
          case policko s.mapa p of
            Kamen ->
              { s | mapa = nastav p Nic s.mapa } |> dalsi
            Laser _ ->
              { s | mapa = nastav p Nic s.mapa } |> dalsi
            Veza _ ->
              { s | mapa = nastav p Nic s.mapa } |> dalsi
            Clovek _ ->
              { s | mapa = nastav p Nic s.mapa } |> dalsi
            Zrkadlo _ _ _ ->
              { s | mapa = nastav p Nic s.mapa } |> dalsi
            _ ->
              { s | mapa = s.mapa |> vyprazdni sur |> poloz (Clovek s.hrac) (smer s kam sur) 0 } |> dalsi


dalsi : Stav -> Stav
dalsi s =
  { s | hrac = modBy s.hracov (s.hrac + 1) }



-- BEHANIE PO MAPE


type alias Poloha =
  { x : Int
  , y : Int
  }


type Smer
  = Sever
  | Vychod
  | Juh
  | Zapad


posunutie : Smer -> Poloha
posunutie kam =
  case kam of
    Sever ->
      Poloha 0 -1
    Vychod ->
      Poloha 1 0
    Juh ->
      Poloha 0 1
    Zapad ->
      Poloha -1 0


smer : Stav -> Smer -> Poloha -> Poloha
smer s kam { x, y } =
  { x = modBy s.sirka (x + (posunutie kam).x), y = modBy s.vyska (y + (posunutie kam).y) }


type Odraz
  = Zostupny
  | Vzostupny


odraz : Odraz -> Smer -> Smer
odraz o kam =
  case o of
    Zostupny ->
      case kam of
        Sever ->
          Zapad
        Vychod ->
          Juh
        Juh ->
          Vychod
        Zapad ->
          Sever
    Vzostupny ->
      case kam of
        Sever ->
          Vychod
        Vychod ->
          Sever
        Juh ->
          Zapad
        Zapad ->
          Juh



-- UPDATE


type Msg
  = Prijal
  | Podal
  | Tahal Tah
  | Konfiguroval Int Int Int
  | Potvrdil Tah
  | Vybral Policko


type Tah
  = Zmapuj Int Int
  | Zrod Int
  | UmiestniSa Poloha
  | Chod Smer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( case msg of
    Prijal ->
      { model | faza = Dumanie Nic }
    Potvrdil t ->
      { model | log = model.log ++ [ t ] }
    Tahal t ->
      { model | log = model.log ++ [ t ], faza = Zaver }
    Podal ->
      { model | faza = Podanie }
    Konfiguroval n s v ->
      { model | faza = Konfiguracia n s v }
    Vybral obj ->
      { model | faza = Dumanie obj }
  , Cmd.none
  )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


miestodrzitel : String -> El.Element msg
miestodrzitel =
  El.text >> El.el [El.width El.fill, El.height El.fill, Bg.color (El.rgb 1 0 0)]


tlacidlo : El.Color -> msg -> El.Element msg -> El.Element msg
tlacidlo farba msg obsah =
  Input.button
    [ El.width El.fill
    , El.height El.fill
    , Font.color (El.rgb 1 1 1)
    , Bg.color farba
    , El.pointer
    , El.focused [ Border.color (El.rgba 0 0 0 0) ]
    ]
    { onPress = Just msg
    , label = obsah
    }


ukaz : Policko -> El.Element msg
ukaz obj =
  case obj of
    Nic ->
      El.text "Nič"
    Clovek n ->
      El.text ("Hráč " ++ String.fromInt n)
    _ ->
      El.text "Niečo"


view : Model -> Html.Html Msg
view model =
  let
    stav =
      List.foldl vykonaj novyStav model.log
    na =
      smer stav
    najdi =
      policko stav.mapa
    aktivny =
      if model.faza == Zaver then
        modBy stav.hracov (stav.hrac - 1)
      else
        stav.hrac
    hracNaTahu =
      stav.hraci |> Array.get aktivny |> Maybe.withDefault novyHrac
    polohaNaTahu =
      polohaHraca stav aktivny
  in
    El.layout [ Font.center, Bg.color (El.rgb 0 0 0) ] <|
    case model.faza of
      Konfiguracia n s v ->
        if stav.hracov == 0 then
          -- voľba počtu hráčov
          El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
            [ tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval (n + 1) s v) (El.text "Zvýšiť počet hráčov")
            , tlacidlo (El.rgb 0.1 0.1 0.1) (Zrod n |> Potvrdil) (El.text (String.fromInt n))
            , tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval (max 1 (n - 1)) s v) (El.text "Znížiť počet hráčov")
            ]
        else if stav.sirka == 0 then
          -- voľba rozmerov mapy
          El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
            [ El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
              [ tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n (s + 1) v) (El.text "Zvýšiť šírku mapy")
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Zmapuj s v |> Potvrdil) (El.text (String.fromInt s))
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n (max 1 (s - 1)) v) (El.text "Znížiť šírku mapy")
              ]
            , El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
              [ tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n s (v + 1)) (El.text "Zvýšiť výšku mapy")
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Zmapuj s v |> Potvrdil) (El.text (String.fromInt v))
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n s (max 1 (v - 1))) (El.text "Znížiť výšku mapy")
              ]
            ]
        else
          -- nový hráč prichádza na ťah
          El.text ("Som hráč " ++ String.fromInt aktivny) |> tlacidlo (El.rgb 0 0.6 1) Prijal
      Podanie ->
        -- nový hráč prichádza na ťah
        El.text ("Som hráč " ++ String.fromInt aktivny) |> tlacidlo (El.rgb 0 0.6 1) Prijal
      Dumanie obj ->
        case polohaNaTahu of
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
          Just p ->
            -- normálny ťah živého hráča
            El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
              [ El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                [ El.el [ El.width El.fill ] El.none
                , p |> na Sever |> najdi |> ukaz |> tlacidlo (El.rgb 0.5 0.5 0.5) (Tahal (Chod Sever))
                , El.el [ El.width El.fill ] El.none
                ]
              , El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                [ p |> na Zapad |> najdi |> ukaz |> tlacidlo (El.rgb 0.5 0.5 0.5) (Tahal (Chod Zapad))
                , p |> najdi |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
                , p |> na Vychod |> najdi |> ukaz |> tlacidlo (El.rgb 0.5 0.5 0.5) (Tahal (Chod Vychod))
                ]
              , El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                [ El.el [ El.width El.fill ] El.none
                , p |> na Juh |> najdi |> ukaz |> tlacidlo (El.rgb 0.5 0.5 0.5) (Tahal (Chod Juh))
                , El.el [ El.width El.fill ] El.none
                ]
              , El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                [ El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                  [ tlacidlo (El.rgb 0.8 0.4 0) (Vybral Kamen) (ukaz Kamen)
                  ]
                ]
              ]
      Zaver ->
        case polohaNaTahu of
          Nothing ->
            -- TODO: hráč medzičasom opäť zomrel
            miestodrzitel "Obrazovka pre hráča mŕtveho na konci svojho ťahu"
          Just p ->
            -- čo hráč vidí po svojom ťahu
            El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
              [ El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                [ El.el [ El.width El.fill ] El.none
                , p |> na Sever |> najdi |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
                , El.el [ El.width El.fill ] El.none
                ]
              , El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                [ p |> na Zapad |> najdi |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
                , p |> najdi |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
                , p |> na Vychod |> najdi |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
                ]
              , El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                [ El.el [ El.width El.fill ] El.none
                , p |> na Juh |> najdi |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
                , El.el [ El.width El.fill ] El.none
                ]
              , El.text ("Podávam hráčovi " ++ String.fromInt stav.hrac) |> tlacidlo (El.rgb 0.4 0.8 0) Podal
              ]
