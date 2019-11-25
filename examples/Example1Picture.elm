module Example1Picture exposing (main)

import Playground exposing (..)
import Playground3d exposing (..)


main : Program () Screen PictureMsg
main =
    picture []
        [ group3d
            [ cube darkPurple purple lightPurple 600
                |> fade3d 0.5
            , words3d darkPurple "3D"
                |> scale3d 5
            ]
            |> shape3dto2d camera1
            |> moveLeft 180
        , group
            [ square darkPurple 300
                |> fade 0.5
            , words darkPurple "2D"
                |> scale 5
            ]
            |> moveRight 180
        ]
