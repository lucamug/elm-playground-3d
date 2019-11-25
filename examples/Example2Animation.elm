module Example2Animation exposing (main)

import Playground exposing (..)
import Playground3d exposing (..)


main : Program () Animation Msg
main =
    animation [] <|
        \time ->
            [ group3d
                [ cube lightPurple purple darkPurple 600
                    |> fade3d 0.5
                , words3d darkPurple "3D"
                    |> scale3d 5
                    |> move3d 300 300 0
                ]
                |> rotate3d 0 0 (spin 3 time)
                |> shape3dto2d camera1
                |> rotate (spin 3 time)
                |> moveLeft 180
            , group
                [ square darkPurple 300
                    |> fade 0.5
                , words darkPurple "2D"
                    |> scale 5
                ]
                |> rotate (spin 3 time)
                |> moveRight 180
            ]
