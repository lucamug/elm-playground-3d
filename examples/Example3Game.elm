module Example3Game exposing (main)

import Playground exposing (..)
import Playground3d exposing (..)
import Set


main : Program () (Game Memory) Msg
main =
    game view update ( 0, 0 )


type alias Memory =
    ( Float, Float )


update : Computer -> Memory -> Memory
update computer ( x, y ) =
    let
        ( dx, dy ) =
            toXY computer.keyboard
    in
    ( x + dx, y + dy )


view : Computer -> Memory -> List Shape
view computer ( x, y ) =
    [ shape3dto2d camera2 <|
        fade3d 0.8 <|
            rotate3d 0 0 (computer.mouse.x / 3) <|
                group3d
                    [ polygon3d lightGray
                        [ ( 0, 0, 0 )
                        , ( 0, 1000, 0 )
                        , ( 0, 1000, 1000 )
                        , ( 0, 0, 1000 )
                        ]
                    , polygon3d grey
                        [ ( 0, 0, 0 )
                        , ( 1000, 0, 0 )
                        , ( 1000, 0, 1000 )
                        , ( 0, 0, 1000 )
                        ]
                    , polygon3d darkGray
                        [ ( 0, 0, 0 )
                        , ( 0, 1000, 0 )
                        , ( 1000, 1000, 0 )
                        , ( 1000, 0, 0 )
                        ]
                    , (if Set.member "d" computer.keyboard.keys then
                        move3d (clamp 100 900 (500 - x * 30)) (clamp 100 900 (500 - y * 30)) 100

                       else
                        move3d (500 - x * 30) (500 - y * 30) (500 + computer.mouse.y * 2)
                      )
                      <|
                        (if Set.member "s" computer.keyboard.keys then
                            identity

                         else
                            rotate3d (spin 5 computer.time) (spin 6 computer.time) (spin 7 computer.time)
                        )
                        <|
                            cube darkPurple purple lightPurple 200
                    , polygon3d black
                        [ ( 0, 1070, 0 )
                        , ( 1, 1070, 0 )
                        , ( 1, 0, 0 )
                        , ( 0, 0, 0 )
                        ]
                    , polygon3d black
                        [ ( 0, 1, 0 )
                        , ( 1070, 1, 0 )
                        , ( 1070, 0, 0 )
                        , ( 0, 0, 0 )
                        ]
                    , polygon3d black
                        [ ( 0, 1, 0 )
                        , ( 0, 1, 1070 )
                        , ( 0, 0, 1070 )
                        , ( 0, 0, 0 )
                        ]
                    , move3d 1100 0 0 <| words3d black "X"
                    , move3d 0 1100 0 <| words3d black "Y"
                    , move3d 0 0 1100 <| words3d black "Z"
                    ]
    , moveDown (computer.screen.top - 30) <| words black <| "Code at https://github.com/lucamug/elm-playground-3d"
    , moveDown (computer.screen.top - 10) <| words black <| "D = bring cube Down, S = Stop the animation, Mouse ⬌ = rotate all, Mouse ⬍ = move cube vertically, Arrow keys = move cube horizontally"
    ]
