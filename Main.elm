module Main exposing (..)

import Html as H
import Html.Events as E
import Time
import Tile exposing (Tile(..), TileGrid)


main =
    H.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Point =
    ( Int, Int )


type alias Model =
    { width : Int
    , height : Int
    , position : Point
    , isTicking : Bool
    , data : List Int
    , tiles : TileGrid
    , selectedTile : Tile
    }


initState =
    { width = 8
    , height = 8
    , position = ( 0, 0 )
    , isTicking = False
    , data = [ 1, 2, 3 ]
    , tiles = Tile.makeGrid 8 8
    , selectedTile = EmptyTile
    }


init =
    ( initState, Cmd.none )



-- ( x, y ) <-> index


indexAtPoint model ( x, y ) =
    x + y * model.width


pointAtIndex model index =
    ( index % model.width, index // model.width )


hasToken model point =
    model.position == point


area model =
    Tile.count model.tiles


tileAt model point =
    Tile.tileAt point model.tiles



-- UPDATE


type Msg
    = Tick
    | ToggleTicking
    | SelectedTile Tile
    | PlacedTile Point


update msg model =
    case msg of
        Tick ->
            ( iff .isTicking updatePosition model, Cmd.none )

        ToggleTicking ->
            ( toggleTicking model, Cmd.none )

        SelectedTile tile ->
            ( setSelectedTile model tile, Cmd.none )

        PlacedTile point ->
            ( placeSelectedTileAt model point, Cmd.none )


iff pred ifTrue x =
    if pred x then
        ifTrue x
    else
        x


incPosition model =
    { model
        | position = (indexAtPoint model model.position) + 1 % (area model) |> (pointAtIndex model)
    }


updateStack model =
    { model | data = Tile.updateStack (tileAt model model.position) model.data }


updatePosition =
    incPosition >> updateStack


toggleTicking model =
    { model | isTicking = not model.isTicking }


setSelectedTile model tile =
    { model | selectedTile = tile }


placeSelectedTileAt model point =
    { model | tiles = Tile.replaceAt model.tiles point model.selectedTile }



-- SUBSCRIPTIONS


subscriptions model =
    Time.every Time.second (always Tick)



-- VIEW


view model =
    H.div []
        [ grid model
        , stack model
        , tileButtons model
        , H.button [ E.onClick ToggleTicking ] [ H.text "tick" ]
        ]



-- grid


fillTo x =
    List.range 0 (x - 1)


mapRange el max f =
    List.map (\x -> el [] <| f x) <| fillTo max


grid model =
    H.table []
        (mapRange H.tr
            model.height
            (\y ->
                (mapRange H.td
                    model.width
                    (\x -> cell model ( x, y ))
                )
            )
        )


cell model pos =
    [ if hasToken model pos then
        H.text "X"
      else
        H.div [ E.onClick <| PlacedTile pos ] [ drawTile <| tileAt model pos ]
    ]


drawTile tile =
    H.text
        (case tile of
            Draw ->
                "+"

            Drop ->
                "-"

            EmptyTile ->
                "_"
        )


stack model =
    H.div [] <| List.map (H.text << toString) model.data


tileOptions =
    [ EmptyTile, Draw, Drop ]


tileButtons model =
    H.div [] <| List.map (tileButton model) tileOptions


tileButton model tile =
    H.button [ E.onClick <| SelectedTile tile ] [ drawTile tile ]
