module Galeria exposing (..)

import Array exposing (Array)
import Html.Styled as Htmls exposing (Html, div, text)
import Html.Styled.Attributes as Attr exposing (class, css)
import Html.Styled.Events as EventS
import Process
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Task


type Amimacion
    = Entra
    | Sale


type AvManual
    = None
    | Izq
    | Der


type alias Model =
    { showSlider : Bool
    , avanzoManual : AvManual
    , inicializado : Bool
    , cualSlideActivo : Int
    , aminar : Amimacion
    , cambia : Int
    , cuantasFotos : Int
    }


newModel : Int -> Model
newModel cFotos =
    { showSlider = False
    , avanzoManual = None
    , inicializado = False
    , cualSlideActivo = 0
    , aminar = Entra
    , cambia = 0
    , cuantasFotos = cFotos
    }


type Msg
    = Avanza
    | Retrocede
    | Para


type Effect
    = EsperaLuegoPara
    | Nada
    | LanzaPara


runEffect : Effect -> Cmd Msg
runEffect efecto =
    case efecto of
        EsperaLuegoPara ->
            Task.perform (\_ -> Para) (Process.sleep 1300)

        Nada ->
            Cmd.none

        LanzaPara ->
            Task.perform (\_ -> Para) (Task.succeed ())



-- (Process.sleep 1)


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        Avanza ->
            ( { model
                | cambia =
                    if model.showSlider then
                        1

                    else
                        0
                , aminar = Sale
                , showSlider = True
              }
            , if model.showSlider then
                EsperaLuegoPara

              else
                LanzaPara
            )

        Retrocede ->
            ( { model
                | cambia =
                    if model.showSlider then
                        -1

                    else
                        0
                , aminar = Sale
                , showSlider = True
              }
            , if model.showSlider then
                EsperaLuegoPara

              else
                LanzaPara
            )

        Para ->
            let
                nuevoSlideActivo : Int
                nuevoSlideActivo =
                    model.cualSlideActivo + model.cambia

                nuevoSlideActivoValidado : Int
                nuevoSlideActivoValidado =
                    if nuevoSlideActivo < 0 then
                        model.cuantasFotos - 1

                    else if nuevoSlideActivo == model.cuantasFotos then
                        0

                    else
                        nuevoSlideActivo
            in
            ( { model
                | cualSlideActivo = nuevoSlideActivoValidado
                , aminar = Entra
                , cambia = 0
              }
            , Nada
            )


view : Array String -> Array String -> Model -> Html Msg
view listadoCompletoImgs textos model =
    div
        [ Attr.id "slider-container" ]
        [ viewSlider
            model.showSlider
            listadoCompletoImgs
            textos
            model.cualSlideActivo
            model.aminar
        ]


viewSlider : Bool -> Array String -> Array String -> Int -> Amimacion -> Html Msg
viewSlider showIt listadoCompletoImgs textos slideActivo animar =
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

        seccionTexto =
            div
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
    in
    div
        [ class "slider" ]
        [ div
            [ class "nav" ]
            [ div
                [ class "next"
                , EventS.onClick Avanza
                ]
                []
            , div
                [ class "prev"
                , EventS.onClick Retrocede
                ]
                []
            , div
                [ class "explore-btn" ]
                [ text "Explore" ]
            , if showIt then
                div
                    [ class "item" ]
                    [ seccionDeImagenes (4 * slideActivo)
                    , seccionTexto
                    ]

              else
                div [ class "item" ]
                    [ seccionTexto
                    ]
            ]
        ]



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
