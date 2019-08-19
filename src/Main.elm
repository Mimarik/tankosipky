import Array
import Browser
import Element as El
import Element.Background as Bg
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Html


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  Maybe Hra


type alias Hra =
  { hracov : Int
  , sirka : Int
  , vyska : Int
  , faza : Faza
  , log : List Tah
  }


type alias Stav =
  { hraci : Array.Array Hrac
  , hrac : Int
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


init : () -> ( Model, Cmd Msg )
init _ =
  ( Nothing, Cmd.none )


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



-- behanie po mape


type alias Poloha =
  { x : Int, y : Int }


type alias Smer =
  Poloha -> Poloha


smer : Hra -> Int -> Int -> Smer
smer { sirka, vyska } dx dy { x, y } =
  { x = modBy sirka (x + dx), y = modBy vyska (y + dy) }


sever : Hra -> Smer
sever m =
  smer m 0 -1


vychod : Hra -> Smer
vychod m =
  smer m 1 0


juh : Hra -> Smer
juh m =
  smer m 0 1


zapad : Hra -> Smer
zapad m =
  smer m -1 0


type alias Odraz =
  Smer -> Smer


odrazV : Hra -> Odraz
odrazV m s p =
  { x = modBy m.sirka (p.x - (s p).y + p.y), y = modBy m.vyska (p.y - (s p).x + p.x) }


odrazZ : Hra -> Odraz
odrazZ m s p =
  { x = modBy m.sirka (p.x + (s p).y - p.y), y = modBy m.vyska (p.y + (s p).x - p.x) }



-- UPDATE


type Msg
  = Prijal
  | Podal
  | Tahal Tah


type Tah
  = UmiestnilSa Poloha


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( case model of
    Nothing ->
      -- TODO: spustenie novej hry
      Nothing
    Just h ->
      case msg of
        Prijal ->
          Just { h | faza = Dumanie }
        Tahal t ->
          Just { h | log = h.log ++ [ t ], faza = Zaver }
        Podal ->
          Just { h | faza = Podanie }
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
   case model of
    Nothing ->
      -- TODO: úvodná obrazovka
      El.none
    Just h ->
      case h.faza of
        Podanie ->
          -- nový hráč prichádza na ťah
          El.text ("Som hráč " ++ String.fromInt hrac) |> El.el (tlacidlo (El.rgb 1 0.6 1) Prijal)
        Dumanie ->
          case Array.get hrac hraci of
            Nothing ->
              -- TODO: chybová stránka (alebo zmena modelu, aby sme nemuseli Array.get)
              El.none
            Just h ->
              case h.sur of
                Nothing ->
                  -- umiestni sa na mape
                  List.range 0 (vyska - 1)
                    |> List.map
                      (\y ->
                        List.range 0 (sirka - 1)
                          |> List.map (\x -> El.el (tlacidlo (El.rgb 0 0 0) (UmiestnilSa (Poloha x y))) El.none)
                          |> El.row [ El.width El.fill, El.height El.fill, El.spacing 8 ]
                      )
                    |> El.column [ El.width El.fill, El.height El.fill, El.spacing 8, El.padding 8 ]

                Just { x, y } ->
                  -- TODO: normálne ťahy
                  El.none
        Zaver ->
          -- TODO: čo hráč vidí po svojom ťahu
          El.none
  )
    |> El.layout [ Font.center ]
