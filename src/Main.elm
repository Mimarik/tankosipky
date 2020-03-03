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


poloz : Poloha -> Policko -> Int -> Stav -> Stav
poloz p obj pokus s =
  if pokus > s.sirka * s.vyska then
    s
  else
    case (obj, policko s.mapa p) of
        (Nic, _) ->
          s
        (_, Sipka kam) ->
          s |> poloz (p |> smer s kam) obj (pokus + 1)
        (Sipka kam, _) ->
          { s | mapa = s.mapa |> nastav p obj } |> poloz (p |> smer s kam) (policko s.mapa p) 0
        (_, Tank _) ->
          s
        (Tank _, _) ->
          { s | mapa = s.mapa |> nastav p obj }
        (Mina, Gula g) ->
          { s | mapa = s.mapa |> nastav p (MinaGula g) }
        (MinaGula _, Gula g) ->
          { s | mapa = s.mapa |> nastav p (MinaGula g) }
        (MinaLuc _, Gula g) ->
          { s | mapa = s.mapa |> nastav p (MinaGula g) }
        (_, Gula _) ->
          s
        (Mina, MinaGula g) ->
          { s | mapa = s.mapa |> nastav p (Gula g) }
        (MinaGula _, MinaGula g) ->
          { s | mapa = s.mapa |> nastav p (Gula g) }
        (MinaLuc _, MinaGula g) ->
          { s | mapa = s.mapa |> nastav p (Gula g) }
        (_, MinaGula _) ->
          s
        (Gula g, Mina) ->
          { s | mapa = s.mapa |> nastav p (MinaGula g) }
        (Gula _, _) ->
          { s | mapa = s.mapa |> nastav p obj }
        (MinaGula g, Mina) ->
          { s | mapa = s.mapa |> nastav p (Gula g) }
        (MinaGula _, _) ->
          { s | mapa = s.mapa |> nastav p obj  }
        (Luc l, Mina) ->
          { s | mapa = s.mapa |> nastav p (MinaLuc l) }
        (_, Mina) ->
          { s | mapa = s.mapa |> nastav p Nic }
        (Luc _, MinaLuc _) ->
          s
        (_, MinaLuc l) ->
          { s | mapa = s.mapa |> nastav p (Luc l) }
        (Mina, Nic) ->
          { s | mapa = s.mapa |> nastav p obj }
        (Mina, Luc l) ->
          { s | mapa = s.mapa |> nastav p (MinaLuc l) }
        (Mina, _) ->
          { s | mapa = s.mapa |> nastav p Nic }
        (MinaLuc _, Nic) ->
          { s | mapa = s.mapa |> nastav p obj }
        (MinaLuc _, Luc _) ->
          { s | mapa = s.mapa |> nastav p obj }
        (MinaLuc l, _) ->
          { s | mapa = s.mapa |> nastav p (Luc l) }
        (_, Kamen) ->
          s
        (Kamen, _) ->
          { s | mapa = s.mapa |> nastav p obj }
        (_, Laser _) ->
          s
        (Laser _, _) ->
          { s | mapa = s.mapa |> nastav p obj }
        (_, Veza _) ->
          s
        (Veza _, _) ->
          { s | mapa = s.mapa |> nastav p obj }
        (Zrkadlo _ _ _, Luc _) ->
          { s | mapa = s.mapa |> nastav p obj }
        (_, Luc _) ->
          s
        (Luc _, Zrkadlo _ _ _) ->
          s
        (Luc _, _) ->
          { s | mapa = s.mapa |> nastav p obj }
        (_, Clovek _) ->
          s
        (Clovek _, _) ->
          { s | mapa = s.mapa |> nastav p obj }
        (Zrkadlo _ _ _, Zrkadlo _ _ _) ->
          s
        (Zrkadlo _ _ _, Nic) ->
          { s | mapa = s.mapa |> nastav p obj }


vyprazdni : Poloha -> Array.Array (Array.Array Policko) -> Array.Array (Array.Array Policko)
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


vykonaj : Tah -> Stav -> Stav
vykonaj t s =
  let
    sur =
      polohaHraca s s.hrac |> Maybe.withDefault (Poloha 0 0)
  in
    case t of
      Zmapuj sirka vyska ->
        { s | sirka = sirka, vyska = vyska, mapa = Array.repeat sirka (Array.repeat vyska Nic) }
      Zrod n ->
        { s | hracov = n, hraci = Array.repeat n novyHrac }
      UmiestniSa p ->
        s |> poloz p (Clovek s.hrac) 0 |> dalsi
      Chod kam ->
        case policko s.mapa (smer s kam sur) of
          Kamen ->
            { s | mapa = nastav (smer s kam sur) Nic s.mapa } |> dalsi
          Laser _ ->
            { s | mapa = nastav (smer s kam sur) Nic s.mapa } |> dalsi
          Veza _ ->
            { s | mapa = nastav (smer s kam sur) Nic s.mapa } |> dalsi
          Clovek _ ->
            { s | mapa = nastav (smer s kam sur) Nic s.mapa } |> dalsi
          Zrkadlo _ _ _ ->
            { s | mapa = nastav (smer s kam sur) Nic s.mapa } |> dalsi
          _ ->
            { s | mapa = s.mapa |> vyprazdni sur } |> poloz (smer s kam sur) (Clovek s.hrac) 0 |> dalsi


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


tlacidlo : El.Color -> Maybe msg -> El.Element msg -> El.Element msg
tlacidlo farba msg obsah =
  Input.button
    [ El.width El.fill
    , El.height El.fill
    , Font.color (El.rgb 1 1 1)
    , Bg.color farba
    , El.focused [ Border.color (El.rgba 0 0 0 0) ]
    ]
    { onPress = msg
    , label = obsah
    }


menoSmeru : Smer -> String
menoSmeru s =
  case s of
    Sever ->
      "sever"
    Juh ->
      "juh"
    Vychod ->
      "východ"
    Zapad ->
      "západ"


menoOdrazu : Odraz -> String
menoOdrazu o =
  case o of
    Zostupny ->
      "zostupne"
    Vzostupny ->
      "vzostupne"


ukaz : Policko -> El.Element msg
ukaz obj =
  case obj of
    Nic ->
      El.text "Nič"
    Clovek n ->
      El.text ("Hráč " ++ String.fromInt n)
    Tank t ->
      El.text ("Tank otočený na " ++ menoSmeru t )
    Gula g ->
      El.text ("Ohnivá guľa letiaca na " ++ menoSmeru g)
    Mina ->
      El.text "Mína"
    MinaGula g ->
      El.text ("Mína a ohnivá guľa letiaca na " ++ menoSmeru g)
    Kamen ->
      El.text "Kameň"
    Laser l ->
      El.text ("Laser žiariaci na " ++ menoSmeru l)
    Veza v ->
      El.text ("Veža mieriaca na " ++ menoSmeru v)
    Zrkadlo o x y ->
      El.text ("Zrkadlo orientované " ++ menoOdrazu o ++ ", " ++ (if x then "" else "ne") ++ "osvetlené zhora, " ++ (if y then "" else "ne") ++ "osvetlené zdola")
    Luc l ->
      El.text ("Lúč smerujúci na " ++ menoSmeru l)
    MinaLuc l ->
      El.text ("Mína a lúč smerujúci na " ++ menoSmeru l)
    Sipka s ->
      El.text ("Šípka ukazujúca na " ++ menoSmeru s)


vyhlad : Stav -> Poloha -> (Smer -> Maybe msg) -> List (El.Attribute msg) -> El.Element msg
vyhlad stav poloha spravaPre atr =
  El.column ([ El.width El.fill, El.height El.fill, El.spacing 8 ] ++ atr)
    [ El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
      [ El.el [ El.width El.fill ] El.none
      , poloha |> smer stav Sever |> policko stav.mapa |> ukaz |> tlacidlo (El.rgb 0.5 0.5 0.5) (spravaPre Sever)
      , El.el [ El.width El.fill ] El.none
      ]
    , El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
      [ poloha |> smer stav Zapad |> policko stav.mapa |> ukaz |> tlacidlo (El.rgb 0.5 0.5 0.5) (spravaPre Zapad)
      , poloha |> policko stav.mapa |> ukaz |> El.el [ El.width El.fill, El.height El.fill, Bg.color (El.rgb 0.5 0.5 0.5) ]
      , poloha |> smer stav Vychod |> policko stav.mapa |> ukaz |> tlacidlo (El.rgb 0.5 0.5 0.5) (spravaPre Vychod)
      ]
    , El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
      [ El.el [ El.width El.fill ] El.none
      , poloha |> smer stav Juh |> policko stav.mapa |> ukaz |> tlacidlo (El.rgb 0.5 0.5 0.5) (spravaPre Juh)
      , El.el [ El.width El.fill ] El.none
      ]
    ]


tInventara : Policko -> Policko -> Bool -> El.Element Msg
tInventara obj vyber akt =
  tlacidlo
    (if obj /= vyber && akt then El.rgb 0.8 0.4 0 else El.rgb 0.5 0.5 0.5)
    (Vybral (if obj /= vyber && akt then vyber else Nic) |> Just)
    (ukaz vyber)


view : Model -> Html.Html Msg
view model =
  let
    stav =
      List.foldl vykonaj novyStav model.log
    aktivny =
      if model.faza == Zaver then
        modBy stav.hracov (stav.hrac - 1)
      else
        stav.hrac
    hracNT =
      stav.hraci |> Array.get aktivny |> Maybe.withDefault novyHrac
    polohaNT =
      polohaHraca stav aktivny
  in
    El.layout [ Font.center, Bg.color (El.rgb 0 0 0) ] <|
    case model.faza of
      Konfiguracia n s v ->
        if stav.hracov == 0 then
          -- voľba počtu hráčov
          El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
            [ tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval (n + 1) s v |> Just) (El.text "Zvýšiť počet hráčov")
            , tlacidlo (El.rgb 0.1 0.1 0.1) (Zrod n |> Potvrdil |> Just) (El.text (String.fromInt n))
            , tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval (max 1 (n - 1)) s v |> Just) (El.text "Znížiť počet hráčov")
            ]
        else if stav.sirka == 0 then
          -- voľba rozmerov mapy
          El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
            [ El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
              [ tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n (s + 1) v |> Just) (El.text "Zvýšiť šírku mapy")
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Zmapuj s v |> Potvrdil |> Just) (El.text (String.fromInt s))
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n (max 1 (s - 1)) v |> Just) (El.text "Znížiť šírku mapy")
              ]
            , El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
              [ tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n s (v + 1) |> Just) (El.text "Zvýšiť výšku mapy")
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Zmapuj s v |> Potvrdil |> Just) (El.text (String.fromInt v))
              , tlacidlo (El.rgb 0.1 0.1 0.1) (Konfiguroval n s (max 1 (v - 1)) |> Just) (El.text "Znížiť výšku mapy")
              ]
            ]
        else
          -- nový hráč prichádza na ťah
          El.text ("Som hráč " ++ String.fromInt aktivny) |> tlacidlo (El.rgb 0 0.6 1) (Just Prijal)
      Podanie ->
        -- nový hráč prichádza na ťah
        El.text ("Som hráč " ++ String.fromInt aktivny) |> tlacidlo (El.rgb 0 0.6 1) (Just Prijal)
      Dumanie obj ->
        case polohaNT of
          Nothing ->
            -- umiestni sa na mape
            List.range 0 (stav.vyska - 1)
              |> List.map
                (\y ->
                  List.range 0 (stav.sirka - 1)
                    |> List.map (\x -> tlacidlo (El.rgb 0.8 0.8 0.8) (Poloha x y |> UmiestniSa |> Tahal |> Just) El.none)
                    |> El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                )
              |> El.column [ El.width El.fill, El.height El.fill, El.spacing 8, El.padding 8 ]
          Just p ->
            -- normálny ťah živého hráča
            El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
              [ vyhlad stav p (Chod >> Tahal >> Just) [ El.height (El.fillPortion 3) ]
              , El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                [ El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ] <|
                  [ El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                    [ El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                      (List.map
                        (\i ->
                          El.el [El.width El.fill, El.height El.fill, Bg.color (if hracNT.kamen >= i then El.rgb 1 1 1 else El.rgb 0.5 0.5 0.5) ] El.none
                        )
                        (List.range 1 10)
                      )
                    , tInventara obj Kamen (hracNT.kamen > 0)
                    ]
                  ] ++
                  List.map (\(o, c) -> tInventara obj o c)
                    [ (Sipka Zapad, hracNT.sipkaZ) , (Sipka Juh, hracNT.sipkaJ), (Sipka Sever, hracNT.sipkaS), (Sipka Vychod, hracNT.sipkaV) ] ++
                  [ El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ] [] ]
                ]
              ]
      Zaver ->
        case polohaNT of
          Nothing ->
            -- TODO: hráč medzičasom opäť zomrel
            miestodrzitel "Obrazovka pre hráča mŕtveho na konci svojho ťahu"
          Just p ->
            -- čo hráč vidí po svojom ťahu
            El.column [ El.width El.fill, El.height El.fill, El.spacing 8 ]
              [ vyhlad stav p (always Nothing) [ El.height (El.fillPortion 3) ]
              , El.text ("Podávam hráčovi " ++ String.fromInt stav.hrac) |> tlacidlo (El.rgb 0.4 0.8 0) (Just Podal)
              ]
