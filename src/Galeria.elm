module Main exposing (..)

import Array exposing (Array)
import Browser
import Css.Global
import Html.Styled as Htmls exposing (Html, div, text)
import Html.Styled.Attributes as Attr exposing (class, css)
import Html.Styled.Events as Event
import Json.Decode as D
import Json.Encode as E
import Process
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw
import Task



-- MAIN


type Amimacion
    = Entra
    | Sale


type alias Model =
    { showSlider : Bool
    , inicializado : Bool
    , cualSlideActivo : Int
    , aminar : Amimacion
    , cambia : Int
    }


init : Model
init =
    { showSlider = False
    , inicializado = False
    , cualSlideActivo = 0
    , aminar = Entra
    , cambia = 0
    }



-- UPDATE


type Msg
    = Avanza
    | Retrocede
    | Para


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Avanza ->
            ( { model
                | cambia = 1
                , aminar = Sale
              }
            , Task.perform (\_ -> Para) (Process.sleep 1300)
            )

        Retrocede ->
            ( { model
                | cambia = -1
                , aminar = Sale
              }
            , Task.perform (\_ -> Para) (Process.sleep 1300)
            )

        Para ->
            ( { model
                | cualSlideActivo = model.cualSlideActivo + model.cambia
                , aminar = Entra
                , cambia = 0
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div
            [ Attr.id "slider-container" ]
            [ viewSlider model.cualSlideActivo model.aminar ]
        ]


listadoCompletoImgs : Array String
listadoCompletoImgs =
    List.map
        (\cual -> "https://picsum.photos/seed/" ++ String.fromChar cual ++ "/700/700")
        (String.toList "abcdefghijklmnopqrst")
        |> Array.fromList


viewSlider slideActivo animar =
    let
        despliega4 : Array String -> List (Html msg)
        despliega4 subListado =
            Array.toIndexedList subListado
                |> List.foldl
                    -- (a -> b -> b) -> b -> List a -> b -//- tuple  -> Html msg
                    (\( indice, direccion ) listadoAc ->
                        div
                            [ class <| "img img-" ++ String.fromInt (indice + 1) ]
                            [ case animar of
                                Sale ->
                                    animatedStyledNode
                                        Htmls.img
                                        (fotoVa indice)
                                        [ Attr.src direccion ]
                                        []

                                Entra ->
                                    animatedStyledNode
                                        Htmls.img
                                        (fotoViene indice)
                                        [ Attr.src direccion ]
                                        []
                            ]
                            :: listadoAc
                    )
                    []

        seccionDeImagenes desdeCual =
            div
                [ class "imgs" ]
                [ div
                    [ class "grid" ]
                  <|
                    despliega4 <|
                        Array.slice
                            desdeCual
                            (desdeCual + 4)
                            listadoCompletoImgs
                ]

        textos : Array String
        textos =
            [ "Uno"
            , "Dos"
            , "tres"
            , "cuatro"
            , "cinco"
            ]
                |> Array.fromList

        despliegaTexto : Html msg
        despliegaTexto =
            div
                [ class "item" ]
                [ div
                    [ class "content" ]
                    [ div
                        [ class "wrap" ]
                        (textos
                            |> Array.get slideActivo
                            |> Maybe.withDefault ""
                            |> String.toList
                            |> List.indexedMap
                                (\indice letra ->
                                    case animar of
                                        Sale ->
                                            animatedStyledNode
                                                Htmls.span
                                                (letraVa indice)
                                                [ class "letter" ]
                                                [ text (String.fromChar letra) ]

                                        Entra ->
                                            animatedStyledNode
                                                Htmls.span
                                                (letraViene indice)
                                                [ class "letter" ]
                                                [ text (String.fromChar letra) ]
                                )
                        )
                    ]
                , seccionDeImagenes (4 * slideActivo)
                ]
    in
    div
        [ class "slider" ]
        [ div
            [ class "nav" ]
            [ div
                [ class "next"
                , Event.onClick Avanza
                ]
                []
            , div
                [ class "prev"
                , Event.onClick Retrocede
                ]
                []
            , div
                [ class "explore-btn" ]
                [ text "Explore" ]
            , despliegaTexto
            ]
        ]



-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg

                else
                    D.fail "some other key"
            )



-- HELPERS ELM SIMPLE ANIMATION


animatedStyledNode :
    (List (Htmls.Attribute msg) -> List (Htmls.Html msg) -> Htmls.Html msg)
    -> Animation
    -> List (Htmls.Attribute msg)
    -> List (Htmls.Html msg)
    -> Htmls.Html msg
animatedStyledNode nodeToAnimate animation attributes children =
    Animated.custom
        (\className stylesheet ->
            nodeToAnimate
                (class className :: attributes)
                (Htmls.node
                    "style"
                    []
                    [ text stylesheet ]
                    :: children
                )
        )
        animation


letraVa : Int -> Animation
letraVa orden =
    Animation.fromTo
        { duration = 400
        , options =
            [ Animation.delay (orden * 70)
            , Animation.easeInQuint
            ]
        }
        [ P.opacity 1
        , P.y 0
        ]
        [ P.opacity 0
        , P.y -60.0
        ]


letraViene : Int -> Animation
letraViene orden =
    Animation.fromTo
        { duration = 600
        , options =
            [ Animation.delay (1000 + orden * 70)
            , Animation.easeOutQuint
            ]
        }
        [ P.opacity 0
        , P.y 60.0
        ]
        [ P.opacity 1
        , P.y 0
        ]


fotoVa : Int -> Animation
fotoVa orden =
    Animation.fromTo
        { duration = 400
        , options =
            [ Animation.delay (500 + 120 * orden)
            , Animation.easeInCubic
            ]
        }
        [ P.opacity 1
        , P.y 0

        --, P.rotate -5
        ]
        [ P.opacity 0

        --, P.rotate -20
        , P.y -600.0
        ]


fotoViene : Int -> Animation
fotoViene orden =
    Animation.fromTo
        { duration = 400
        , options =
            [ Animation.delay (200 * orden)
            , Animation.easeInCubic
            ]
        }
        [ P.opacity 0
        , P.y 600

        --, P.rotate -20
        ]
        [ P.opacity 1

        --, P.rotate -5
        , P.y 0
        ]
