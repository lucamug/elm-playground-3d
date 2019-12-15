module Playground3d exposing
    ( Point2d
    , Point3d
    , camera
    , camera1
    , camera2
    , camera3
    , camera4
    , cube
    , fade3d
    , group3d
    , move3d
    , polygon3d
    , rotate3d
    , scale3d
    , shape3dto2d
    , words3d
    )

import Angle
import Axis3d
import Camera3d
import Direction3d
import Length
import Playground
import Point2d
import Point3d
import Point3d.Projection
import Rectangle2d
import Vector3d
import Viewpoint3d


type alias Triplet a =
    ( a, a, a )


type alias Position =
    Triplet Playground.Number


type alias Delta3d =
    Triplet Playground.Number


type alias Fade =
    Playground.Number


type alias Scale =
    Playground.Number


type Shape3d
    = Shape3d Fade Form3d


type Form3d
    = Polygon3d Playground.Color (List Position)
    | Words3d Playground.Color Position Scale String
    | Group3d (List Shape3d)


polygon3d : Playground.Color -> List Position -> Shape3d
polygon3d color points =
    Shape3d 1 (Polygon3d color points)


words3d : Playground.Color -> String -> Shape3d
words3d color string =
    Shape3d 1 (Words3d color ( 0, 0, 0 ) 1 string)


group3d : List Shape3d -> Shape3d
group3d shapes =
    Shape3d 1 (Group3d shapes)


cube : Playground.Color -> Playground.Color -> Playground.Color -> Playground.Number -> Shape3d
cube color1 color2 color3 size =
    scale3d size <|
        move3d -0.5 -0.5 -0.5 <|
            group3d
                [ polygon3d color1 [ ( 0, 0, 0 ), ( 1, 0, 0 ), ( 1, 1, 0 ), ( 0, 1, 0 ) ]
                , polygon3d color2 [ ( 0, 0, 0 ), ( 0, 1, 0 ), ( 0, 1, 1 ), ( 0, 0, 1 ) ]
                , polygon3d color3 [ ( 0, 0, 0 ), ( 1, 0, 0 ), ( 1, 0, 1 ), ( 0, 0, 1 ) ]
                , polygon3d color1 [ ( 0, 0, 1 ), ( 1, 0, 1 ), ( 1, 1, 1 ), ( 0, 1, 1 ) ]
                , polygon3d color2 [ ( 0, 1, 0 ), ( 1, 1, 0 ), ( 1, 1, 1 ), ( 0, 1, 1 ) ]
                , polygon3d color3 [ ( 1, 0, 0 ), ( 1, 1, 0 ), ( 1, 1, 1 ), ( 1, 0, 1 ) ]
                ]


geometryMove : Delta3d -> Point3d a -> Point3d a
geometryMove ( dx, dy, dz ) point3d =
    Point3d.translateBy (Vector3d.meters dx dy dz) point3d


geometryScale : Delta3d -> Point3d a -> Point3d a
geometryScale ( dx, _, _ ) point3d =
    Point3d.scaleAbout (Point3d.meters 0 0 0) dx point3d


geometryRotate : Delta3d -> Point3d a -> Point3d a
geometryRotate ( dx, dy, dz ) point3d =
    point3d
        |> Point3d.rotateAround Axis3d.x (Angle.degrees dx)
        |> Point3d.rotateAround Axis3d.y (Angle.degrees dy)
        |> Point3d.rotateAround Axis3d.z (Angle.degrees dz)


tripletTransformation :
    (Delta3d -> Point3d a -> Point3d a)
    -> Float
    -> Float
    -> Float
    -> Position
    -> Position
tripletTransformation geometryTransform dx dy dz ( px, py, pz ) =
    Point3d.toTuple Length.inMeters <|
        geometryTransform ( dx, dy, dz ) (Point3d.meters px py pz)


move3d : Playground.Number -> Playground.Number -> Playground.Number -> Shape3d -> Shape3d
move3d dx dy dz (Shape3d o f) =
    Shape3d o <|
        case f of
            Polygon3d color positions ->
                Polygon3d color <| List.map (tripletTransformation geometryMove dx dy dz) positions

            Words3d color position scale string ->
                Words3d color (tripletTransformation geometryMove dx dy dz position) scale string

            Group3d shapes ->
                Group3d <| List.map (move3d dx dy dz) shapes


scale3d : Playground.Number -> Shape3d -> Shape3d
scale3d dx (Shape3d o f) =
    Shape3d o <|
        case f of
            Polygon3d color position ->
                Polygon3d color <| List.map (tripletTransformation geometryScale dx dx dx) position

            Words3d color position scale string ->
                Words3d color position (scale * dx) string

            Group3d shapes ->
                Group3d <| List.map (\shape -> scale3d dx shape) shapes


rotate3d : Playground.Number -> Playground.Number -> Playground.Number -> Shape3d -> Shape3d
rotate3d dx dy dz (Shape3d o f) =
    Shape3d o <|
        case f of
            Polygon3d color positions ->
                Polygon3d color <| List.map (tripletTransformation geometryRotate dx dy dz) positions

            Words3d color position scale string ->
                Words3d color (tripletTransformation geometryRotate dx dy dz position) scale string

            Group3d shapes ->
                Group3d <| List.map (\shape -> rotate3d dx dy dz shape) shapes


fade3d : Fade -> Shape3d -> Shape3d
fade3d o (Shape3d _ f) =
    case f of
        Polygon3d _ _ ->
            Shape3d o f

        Words3d _ _ _ _ ->
            Shape3d o f

        Group3d shapes ->
            Shape3d o <| Group3d <| List.map (fade3d o) shapes


type alias Point3d a =
    Point3d.Point3d Length.Meters a


type alias Point2d a =
    Point2d.Point2d Length.Meters a


point3dto2d : (Point3d a -> Maybe (Point2d a)) -> Position -> Maybe ( Float, Float )
point3dto2d camera_ ( x, y, z ) =
    Point3d.meters x y z
        |> camera_
        |> Maybe.map (Point2d.toTuple Length.inMeters)


removeMaybeType : Maybe a -> List a -> List a
removeMaybeType item list =
    case item of
        Nothing ->
            list

        Just v ->
            v :: list


shape3dto2d : (Point3d a -> Maybe (Point2d a)) -> Shape3d -> Playground.Shape
shape3dto2d camera_ (Shape3d o f) =
    case f of
        Polygon3d color listPoints ->
            Playground.polygon color
                (listPoints
                    |> List.map (point3dto2d camera_)
                    |> List.foldr removeMaybeType []
                )
                |> Playground.fade o

        Words3d color position scale string ->
            let
                ( x, y ) =
                    case point3dto2d camera_ position of
                        Just point ->
                            point

                        Nothing ->
                            ( 0, 0 )
            in
            Playground.words color string
                |> Playground.move x (0 - y)
                |> Playground.scale scale
                |> Playground.fade o

        Group3d shapes ->
            Playground.group <| List.map (shape3dto2d camera_) shapes



--  ██████  █████  ███    ███ ███████ ██████   █████
-- ██      ██   ██ ████  ████ ██      ██   ██ ██   ██
-- ██      ███████ ██ ████ ██ █████   ██████  ███████
-- ██      ██   ██ ██  ██  ██ ██      ██   ██ ██   ██
--  ██████ ██   ██ ██      ██ ███████ ██   ██ ██   ██


type alias EyePoint =
    Triplet Float


type alias FocalPoint =
    Triplet Float


camera : EyePoint -> FocalPoint -> Point3d a -> Maybe (Point2d a)
camera ( x, y, z ) ( fx, fy, fz ) point3d =
    let
        cameraViewpoint : Viewpoint3d.Viewpoint3d Length.Meters a
        cameraViewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters x y z
                , focalPoint = Point3d.meters fx fy fz
                , upDirection = Direction3d.positiveZ
                }

        perspectiveCamera : Camera3d.Camera3d Length.Meters a
        perspectiveCamera =
            Camera3d.perspective
                { viewpoint = cameraViewpoint
                , verticalFieldOfView = Angle.degrees 25
                , clipDepth = Length.meters 1
                }

        rectOfView : Rectangle2d.Rectangle2d Length.Meters a
        rectOfView =
            Rectangle2d.with
                { x1 = Length.meters 0
                , y1 = Length.meters 600
                , x2 = Length.meters 1
                , y2 = Length.meters 0
                }
    in
    Point3d.Projection.toScreenSpace perspectiveCamera rectOfView point3d


camera1 : Point3d a -> Maybe (Point2d a)
camera1 =
    camera ( 3000, 2500, 1450 ) ( 0, 0, -1100 )


camera2 : Point3d a -> Maybe (Point2d a)
camera2 =
    camera ( 2300, 2300, 800 ) ( 0, 0, -450 )


camera3 : Point3d a -> Maybe (Point2d a)
camera3 =
    camera ( 2000, 2000, 1500 ) ( 0, 1.6, -908 )


camera4 : Point3d a -> Maybe (Point2d a)
camera4 =
    camera ( 50, 50, 50 ) ( -50, -50, -50 )
