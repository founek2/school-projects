port module TodoList exposing (main)

import Browser
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List
import Styles
import WebDataTypes exposing (..)


port newTodo : (Todo -> msg) -> Sub msg


port userIdChange : (Int -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { todos : WebData TodoList
    , updateTodo : UpdateData Todo
    , deleteTodo : UpdateData Todo
    }


type alias Todo =
    { id : Int
    , todo : String
    , completed : Bool
    , userId : Int
    }


todoDecoder : Decode.Decoder Todo
todoDecoder =
    Decode.succeed Todo
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "todo" Decode.string
        |> Pipeline.required "completed" Decode.bool
        |> Pipeline.required "userId" Decode.int


todoEncoder : Todo -> Encode.Value
todoEncoder todo =
    Encode.object
        [ ( "todo", Encode.string todo.todo )
        , ( "completed", Encode.bool todo.completed )
        , ( "userId", Encode.int todo.userId )
        ]


type alias TodoList =
    List Todo


todoListDecoder : Decode.Decoder TodoList
todoListDecoder =
    Decode.list todoDecoder


type alias TodoListPaginated =
    { todos : TodoList
    , total : Int
    , skip : Int
    , limit : Int
    }


todoListPaginatedDecoder : Decode.Decoder TodoListPaginated
todoListPaginatedDecoder =
    Decode.succeed TodoListPaginated
        |> Pipeline.required "todos" todoListDecoder
        |> Pipeline.required "total" Decode.int
        |> Pipeline.required "skip" Decode.int
        |> Pipeline.required "limit" Decode.int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { todos = NotAsked
      , updateTodo = UpdateNotAsked
      , deleteTodo = UpdateNotAsked
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ newTodo RecieveTodo, userIdChange RecieveUserId ]



-- API ---- ----- ----- -----


baseApiUrl : String
baseApiUrl =
    "https://dummyjson.com"


getTodos : Int -> Cmd Msg
getTodos userId =
    Http.get
        { url = baseApiUrl ++ "/todos/user/" ++ String.fromInt userId
        , expect = Http.expectJson GotTodos todoListPaginatedDecoder
        }


completeTodoFetch : Todo -> Cmd Msg
completeTodoFetch todo =
    Http.request
        { method = "PATCH"

        -- Adding Content-type application/json was duplicating value on Firefox and causing invalid value
        , headers = []
        , url = baseApiUrl ++ "/todos/" ++ String.fromInt todo.id
        , body =
            Http.jsonBody <|
                todoEncoder
                    { todo
                        | completed =
                            if todo.completed then
                                False

                            else
                                True
                    }
        , expect = Http.expectJson GotTodo todoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteTodoFetch : Todo -> Cmd Msg
deleteTodoFetch todo =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = baseApiUrl ++ "/todos/" ++ String.fromInt todo.id
        , body = Http.emptyBody
        , expect = Http.expectJson DeletedTodo todoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- ### ---- ----- ----- -----


type Msg
    = GotTodos (Result Http.Error TodoListPaginated)
    | RecieveTodo Todo
    | RecieveUserId Int
    | CompleteTodo Todo
    | DeleteTodo Todo
    | GotTodo (Result Http.Error Todo)
    | DeletedTodo (Result Http.Error Todo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTodos result ->
            case result of
                Ok data ->
                    ( { model | todos = Success data.todos }, Cmd.none )

                Err err ->
                    ( { model | todos = Failure err }, Cmd.none )

        RecieveTodo todo ->
            case model.todos of
                Success todos ->
                    ( { model | todos = Success (todo :: todos) }, Cmd.none )

                NotAsked ->
                    ( { model | todos = Success [ todo ] }, Cmd.none )

                Failure _ ->
                    ( { model | todos = Success [ todo ] }, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

        RecieveUserId userId ->
            ( { model | todos = Loading }, getTodos userId )

        CompleteTodo todo ->
            ( { model | updateTodo = UpdateLoading todo.id }, completeTodoFetch todo )

        GotTodo result ->
            case result of
                Ok todo ->
                    let
                        todos =
                            case model.todos of
                                Success todosData ->
                                    Success (todosData |> List.map (replaceItem todo))

                                Failure err ->
                                    Failure err

                                NotAsked ->
                                    NotAsked

                                Loading ->
                                    Loading
                    in
                    ( { model | updateTodo = UpdateSucess todo, todos = todos }, Cmd.none )

                Err err ->
                    ( { model | updateTodo = UpdateFailure err }, Cmd.none )

        DeleteTodo todo ->
            ( { model | deleteTodo = UpdateLoading todo.id }, deleteTodoFetch todo )

        DeletedTodo result ->
            case result of
                Ok todo ->
                    let
                        todos =
                            case model.todos of
                                Success todosData ->
                                    Success (todosData |> List.filter (\t -> t.id /= todo.id))

                                Failure err ->
                                    Failure err

                                NotAsked ->
                                    Success [ todo ]

                                Loading ->
                                    Loading
                    in
                    ( { model | deleteTodo = UpdateSucess todo, todos = todos }, Cmd.none )

                Err err ->
                    ( { model | deleteTodo = UpdateFailure err }, Cmd.none )


view : Model -> Html.Html Msg
view { todos, updateTodo, deleteTodo } =
    let
        todosHtml =
            case todos of
                NotAsked ->
                    [ viewNotAsked ]

                Loading ->
                    [ viewLoader ]

                Success data ->
                    data
                        |> List.map
                            (\todo ->
                                viewTodo
                                    (if updateTodo == UpdateLoading todo.id || deleteTodo == UpdateLoading todo.id then
                                        True

                                     else
                                        False
                                    )
                                    todo
                            )

                Failure err ->
                    [ viewFailure err ]
    in
    Html.div []
        (Html.h3 [] [ Html.text "Todos" ] :: todosHtml)


viewNotAsked : Html.Html Msg
viewNotAsked =
    Html.text "Not requested"


viewLoader : Html.Html Msg
viewLoader =
    Html.text "Loading..."


viewFailure : Http.Error -> Html.Html Msg
viewFailure _ =
    Html.text "Failed"


viewTodo : Bool -> Todo -> Html.Html Msg
viewTodo disabled ({ id, todo, completed } as todoItem) =
    Html.div Styles.todoItem
        [ Html.input [ Events.onClick (CompleteTodo todoItem), Attributes.type_ "checkbox", Attributes.id (String.fromInt id), Attributes.checked completed, Attributes.disabled disabled ] []
        , Html.label [] [ Html.text todo, Html.text " " ]
        , Html.button [ Events.onClick (DeleteTodo todoItem), Attributes.disabled disabled ] [ Html.text "delete" ]
        ]


replaceItem : Todo -> Todo -> Todo
replaceItem replacement todo =
    if replacement.id == todo.id then
        replacement

    else
        todo
