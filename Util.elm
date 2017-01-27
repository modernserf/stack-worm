module Util exposing (..)


minRange x =
    List.range 0 <| x - 1


cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


(<<<) =
    (<<) << (<<)


(>>>) =
    (>>) >> (>>)
