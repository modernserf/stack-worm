module Main exposing (..)

import Time
import Html
import Tile exposing (Tile(..), TileGrid)
import Point exposing (Point)
import View exposing (view)
import Msg exposing (Msg(..))
import Model exposing (..)
import Tuple exposing (mapFirst)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update msg model =
    case msg of
        Tick ->
            if model.isTicking then
                updatePosition model
            else
                ( model, Cmd.none )

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
        | position = (Point.toIndex model.width model.position) + 1 % (area model) |> (Point.fromIndex model.width)
    }


updateStack model =
    { model | data = Tile.updateStack (tileAt model model.position) model.data }


updatePosition model =
    ( model, Cmd.none )
        |> mapFirst incPosition
        |> mapFirst updateStack


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
