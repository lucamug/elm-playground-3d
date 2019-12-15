module Examples exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes
import Playground exposing (..)
import Playground3d exposing (..)
import Set


fontXxxLarge : Int
fontXxxLarge =
    30


attrsTitle : List (Attr () msg)
attrsTitle =
    [ Font.size fontXxxLarge
    , Font.color colorFontTitle
    , Font.bold
    ]


colorBackgroundCard : Element.Color
colorBackgroundCard =
    Element.rgb 1 1 1


colorFontTitle : Element.Color
colorFontTitle =
    Element.rgb 0 0.4 0.5


colorBorder : Element.Color
colorBorder =
    Element.rgba 0 0 0 0.2


colorFontNormal : Element.Color
colorFontNormal =
    Element.rgba 0 0 0 0.8


cardBase : List (Attribute msg)
cardBase =
    [ padding 30
    , Background.color colorBackgroundCard
    , Border.rounded 10
    , Border.width 1
    , Border.color colorBorder
    , width fill
    , height fill
    , htmlAttribute <| Html.Attributes.style "transition" "all 0.2s"
    , Font.color colorFontNormal
    ]


shadowHigh : Attr decorative msg
shadowHigh =
    Border.shadow
        { color = rgba255 0 0 0 0.3
        , offset = ( 0, 10 )
        , blur = 10
        , size = 1
        }


shadowLow : Attr decorative msg
shadowLow =
    Border.shadow
        { color = rgba255 0 0 0 0.1
        , offset = ( 0, 0 )
        , blur = 3
        , size = 1
        }


cardNormal : List (Attribute msg)
cardNormal =
    cardBase
        ++ [ shadowLow, mouseOver [ shadowHigh ] ]


paddingNormal : Int
paddingNormal =
    15


spacingNormal : Int
spacingNormal =
    15


view3d1 : (Playground.Msg -> msg) -> Playground.Game memory -> Element msg
view3d1 msgGraph modelGraph =
    row []
        [ paragraph [ paddingEach { top = paddingNormal, right = 0, bottom = 0, left = paddingNormal } ] [ text "3D Example 1" ]
        , el
            (cardNormal
                ++ [ padding 0
                   , width fill
                   ]
            )
          <|
            html <|
                Html.map msgGraph <|
                    Html.div [ Html.Attributes.style "cursor" "move" ]
                        (.body <|
                            let
                                colorCube =
                                    Playground3d.cube (Playground.rgb 250 86 128) Playground.purple (Playground.rgb 290 126 168)
                            in
                            Playground.gameView
                                (\computer a ->
                                    [ [ colorCube 300
                                      , colorCube 300 |> Playground3d.move3d 0 300 0
                                      , colorCube 300 |> Playground3d.move3d 0 300 300
                                      , colorCube 300 |> Playground3d.move3d 0 600 0
                                      , colorCube 300 |> Playground3d.move3d 0 600 300
                                      , colorCube 300 |> Playground3d.move3d 0 600 600
                                      ]
                                        |> Playground3d.group3d
                                        |> Playground3d.move3d 0 -300 -200
                                        |> (if computer.mouse.down then
                                                Playground3d.rotate3d computer.mouse.y computer.mouse.x 0

                                            else
                                                Playground3d.rotate3d 0 0 (Playground.spin 15 computer.time)
                                           )
                                        |> Playground3d.fade3d 0.5
                                        |> Playground3d.shape3dto2d Playground3d.camera1
                                        |> Playground.scale 1.3
                                    ]
                                )
                                modelGraph
                        )
        ]


view3d2 : (Playground.Msg -> msg) -> Playground.Game memory -> Element msg
view3d2 msgGraph modelGraph =
    row []
        [ paragraph [ paddingEach { top = paddingNormal, right = 0, bottom = 0, left = paddingNormal } ] [ text "3D Example 2" ]
        , el (cardNormal ++ [ padding 0, width fill ]) <|
            html <|
                Html.map msgGraph <|
                    Html.div [ Html.Attributes.style "cursor" "move" ]
                        (.body <|
                            let
                                colorCube =
                                    Playground3d.cube Playground.darkGreen Playground.green Playground.yellow
                            in
                            Playground.gameView
                                (\computer a ->
                                    [ colorCube 300
                                        |> Playground3d.scale3d 2
                                        |> Playground3d.rotate3d 0 0 computer.mouse.x
                                        |> Playground3d.fade3d 0.5
                                        |> Playground3d.shape3dto2d Playground3d.camera1
                                    ]
                                )
                                modelGraph
                        )
        ]


view3d3 : (Playground.Msg -> msg) -> Playground.Game memory -> Element msg
view3d3 msgGraph modelGraph =
    row []
        [ paragraph [ paddingEach { top = paddingNormal, right = 0, bottom = 0, left = paddingNormal } ] [ text "3D Example 2" ]
        , el (cardNormal ++ [ padding 0, width fill ]) <|
            html <|
                Html.map msgGraph <|
                    Html.div [ Html.Attributes.style "cursor" "move" ]
                        (.body <|
                            let
                                colorCube =
                                    Playground3d.cube Playground.darkGreen Playground.green Playground.yellow
                            in
                            Playground.gameView
                                (\computer a ->
                                    [ Playground.square Playground.green 200
                                        |> Playground.rotate (spin 4 computer.time)
                                    ]
                                )
                                modelGraph
                        )
        ]


type alias Model =
    { graph1 : Playground.Game ()
    , graph2 : Playground.Game ()
    , graph3 : Playground.Game ()
    , isFocused : Bool
    , isVisible : Bool
    , isMoving : Bool
    }


type Msg
    = Graph1 Playground.Msg
    | Graph2 Playground.Msg
    | Graph3 Playground.Msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


mode : Attributes
mode =
    fixedScreen ( 600, 600 )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( model, cmd ) =
            Playground.gameInit [ mode ] () ()
    in
    ( { graph1 = model
      , graph2 = model
      , graph3 = model
      , isFocused = True
      , isVisible = True
      , isMoving = True
      }
    , Cmd.batch
        [ Cmd.map Graph1 cmd
        , Cmd.map Graph2 cmd
        , Cmd.map Graph3 cmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Graph1 msgGraph ->
            let
                updateMemory computer memory =
                    memory

                ( graphModel, graphCmd ) =
                    Playground.gameUpdate updateMemory msgGraph model.graph1
            in
            ( { model | graph1 = graphModel }, graphCmd )

        Graph2 msgGraph ->
            let
                updateMemory computer memory =
                    memory

                ( graphModel, graphCmd ) =
                    Playground.gameUpdate updateMemory msgGraph model.graph2
            in
            ( { model | graph2 = graphModel }, graphCmd )

        Graph3 msgGraph ->
            let
                updateMemory computer memory =
                    memory

                ( graphModel, graphCmd ) =
                    Playground.gameUpdate updateMemory msgGraph model.graph3
            in
            ( { model | graph3 = graphModel }, graphCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isFocused && model.isVisible && model.isMoving then
        Sub.batch
            [ Sub.map Graph1 <| Playground.gameSubscriptions [ mode ] model.graph1
            , Sub.map Graph2 <| Playground.gameSubscriptions [ mode ] model.graph2
            , Sub.map Graph3 <| Playground.gameSubscriptions [ mode ] model.graph3
            ]

    else
        Sub.none


twoD : String -> List Shape -> Element msg
twoD code shapes =
    row [ width <| px 400 ]
        [ paragraph [] [ text code ]
        , el [ alignRight, width <| px 200 ] <|
            html <|
                Html.div [] <|
                    .body
                        (pictureView
                            [ group
                                [ circle gray 4
                                , circle white 3.8
                                , rectangle gray 10 0.2 |> Playground.rotate 90 |> move -5 0
                                , rectangle gray 10 0.1 |> Playground.rotate 90 |> move -4 0
                                , rectangle gray 10 0.1 |> Playground.rotate 90 |> move -3 0
                                , rectangle gray 10 0.1 |> Playground.rotate 90 |> move -2 0
                                , rectangle gray 10 0.1 |> Playground.rotate 90 |> move -1 0
                                , rectangle gray 10 0.2 |> Playground.rotate 90
                                , rectangle gray 10 0.1 |> Playground.rotate 90 |> move 1 0
                                , rectangle gray 10 0.1 |> Playground.rotate 90 |> move 2 0
                                , rectangle gray 10 0.1 |> Playground.rotate 90 |> move 3 0
                                , rectangle gray 10 0.1 |> Playground.rotate 90 |> move 4 0
                                , rectangle gray 10 0.2 |> Playground.rotate 90 |> move 5 0
                                , rectangle gray 10 0.2 |> move 0 -5
                                , rectangle gray 10 0.1 |> move 0 -4
                                , rectangle gray 10 0.1 |> move 0 -3
                                , rectangle gray 10 0.1 |> move 0 -2
                                , rectangle gray 10 0.1 |> move 0 -1
                                , rectangle gray 10 0.2
                                , rectangle gray 10 0.1 |> move 0 1
                                , rectangle gray 10 0.1 |> move 0 2
                                , rectangle gray 10 0.1 |> move 0 3
                                , rectangle gray 10 0.1 |> move 0 4
                                , rectangle gray 10 0.2 |> move 0 5
                                , words gray "-5,-5" |> Playground.scale 0.06 |> move -4.5 -5.8
                                , words gray "-5, 5" |> Playground.scale 0.06 |> move -4.5 5.8
                                , words gray " 5, 5" |> Playground.scale 0.06 |> move 4.5 5.8
                                , words gray " 5,-5" |> Playground.scale 0.06 |> move 4.5 -5.8
                                ]
                            , group shapes |> fade 0.7
                            ]
                            (toScreen 14 14)
                        )
        ]



-- TODO
--
-- Need to convert the grid into 3D so I can put object in 3D
-- Create Rectangle3d


grid =
    [ circle gray 4
    , circle white 3.8
    , rectangle gray 10 0.2 |> Playground.rotate 90 |> move -5 0
    , rectangle gray 10 0.1 |> Playground.rotate 90 |> move -4 0
    , rectangle gray 10 0.1 |> Playground.rotate 90 |> move -3 0
    , rectangle gray 10 0.1 |> Playground.rotate 90 |> move -2 0
    , rectangle gray 10 0.1 |> Playground.rotate 90 |> move -1 0
    , rectangle gray 10 0.2 |> Playground.rotate 90
    , rectangle gray 10 0.1 |> Playground.rotate 90 |> move 1 0
    , rectangle gray 10 0.1 |> Playground.rotate 90 |> move 2 0
    , rectangle gray 10 0.1 |> Playground.rotate 90 |> move 3 0
    , rectangle gray 10 0.1 |> Playground.rotate 90 |> move 4 0
    , rectangle gray 10 0.2 |> Playground.rotate 90 |> move 5 0
    , rectangle gray 10 0.2 |> move 0 -5
    , rectangle gray 10 0.1 |> move 0 -4
    , rectangle gray 10 0.1 |> move 0 -3
    , rectangle gray 10 0.1 |> move 0 -2
    , rectangle gray 10 0.1 |> move 0 -1
    , rectangle gray 10 0.2
    , rectangle gray 10 0.1 |> move 0 1
    , rectangle gray 10 0.1 |> move 0 2
    , rectangle gray 10 0.1 |> move 0 3
    , rectangle gray 10 0.1 |> move 0 4
    , rectangle gray 10 0.2 |> move 0 5
    , words gray "-5,-5" |> Playground.scale 0.06 |> move -4.5 -5.8
    , words gray "-5, 5" |> Playground.scale 0.06 |> move -4.5 5.8
    , words gray " 5, 5" |> Playground.scale 0.06 |> move 4.5 5.8
    , words gray " 5,-5" |> Playground.scale 0.06 |> move 4.5 -5.8
    ]


threeD : String -> List Shape -> Element msg
threeD code shapes =
    row [ width <| px 400 ]
        [ paragraph [] [ text code ]
        , el [ alignRight, width <| px 200 ] <|
            html <|
                Html.div [] <|
                    .body
                        (pictureView
                            [ group grid
                            , group shapes |> fade 0.7
                            ]
                            (toScreen 14 14)
                        )
        ]


view : Model -> Html.Html Msg
view model =
    let
        contentShape3d1 =
            view3d1 Graph1 model.graph1

        contentShape3d2 =
            view3d2 Graph2 model.graph2

        contentShape3d3 =
            view3d3 Graph3 model.graph3
    in
    layout [ padding paddingNormal ] <|
        column [ spacing spacingNormal, width fill ]
            [ el attrsTitle <| text "Example"
            , column [ spacing spacingNormal, width fill ]
                [ twoD "circle blue 4" [ circle blue 4 ]
                , twoD "oval blue 8 6" [ oval blue 8 6 ]
                , twoD "square blue 8" [ square blue 8 ]
                , twoD "rectangle blue 8 6" [ rectangle blue 8 6 ]
                , twoD "triangle blue 4" [ triangle blue 4 ]
                , twoD "pentagon blue 4" [ pentagon blue 4 ]
                , twoD "hexagon blue 4" [ hexagon blue 4 ]
                , twoD "octagon blue 4" [ octagon blue 4 ]
                , twoD "polygon blue [ (-4, 0), (-1, -1), (0, -4), (1, -1), (4, 0), (1, 1), (0, 4), (-1, 1) ]"
                    [ polygon blue
                        [ ( -4, 0 )
                        , ( -1, -1 )
                        , ( 0, -4 )
                        , ( 1, -1 )
                        , ( 4, 0 )
                        , ( 1, 1 )
                        , ( 0, 4 )
                        , ( -1, 1 )
                        ]
                    ]
                , twoD "words \"test\"" [ words blue "test" |> Playground.scale 0.3 ]
                , threeD "3D test" [ cube lightBlue blue darkBlue 8 |> shape3dto2d camera3 ]
                , threeD "3D test" <|
                    let
                        size =
                            40
                    in
                    [ [ polygon3d darkGray [ ( 0, 0, 0 ), ( size, 0, 0 ), ( size, 0, size ), ( 0, 0, size ) ]

                      -- , polygon3d gray [ ( 0, 0, 0 ), ( 0, size, 0 ), ( 0, size, size ), ( 0, 0, size ) ]
                      -- , polygon3d lightGray [ ( 0, 0, 0 ), ( size, 0, 0 ), ( size, size, 0 ), ( 0, size, 0 ) ]
                      ]
                        |> group3d
                        |> shape3dto2d camera3
                    ]
                , contentShape3d1
                , contentShape3d2
                , contentShape3d3
                ]
            ]
