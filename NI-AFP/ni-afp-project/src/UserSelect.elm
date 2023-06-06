port module UserSelect exposing (main)

import Browser
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import List


port changeUserId : Int -> Cmd msg


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Model =
    Int


init : Int -> ( Model, Cmd Msg )
init defaultUserId =
    ( defaultUserId
    , changeUserId defaultUserId
    )


type Msg
    = ChangeUserId String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg currentUserId =
    case msg of
        ChangeUserId userIdStr ->
            let
                userId =
                    String.toInt userIdStr |> Maybe.withDefault currentUserId
            in
            ( userId, changeUserId userId )


userIDs : List Int
userIDs =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]


view : Model -> Html.Html Msg
view userId =
    Html.select [ Events.onInput ChangeUserId ] (userIDs |> List.map (optionView userId))


optionView : Int -> Int -> Html.Html Msg
optionView selected id =
    let
        idStr =
            String.fromInt id
    in
    Html.option [ Attributes.value idStr, Attributes.selected (selected == id) ] [ Html.text idStr ]
