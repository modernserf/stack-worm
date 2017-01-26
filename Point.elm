module Point exposing (..)


type alias Point =
    ( Int, Int )


toIndex width ( x, y ) =
    x + y * width


fromIndex width index =
    ( index % width, index // width )
