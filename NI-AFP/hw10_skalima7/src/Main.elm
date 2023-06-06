module Main exposing (main)

import Browser
import Dropdown
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Styles


type WebData a
    = NotAsked
    | Loading
    | Failure Http.Error
    | Success a


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { personDropdown : PersonDropdown
    , persons : WebData PersonList
    , records : WebData RecordList
    , deleteResult : WebData ()
    , addResult : WebData ()
    , form : RecordForm
    }


type alias PersonDropdown =
    { state : Dropdown.State
    , selectedPerson : Maybe Person
    }


type alias Person =
    { id : String
    , firstName : String
    , lastName : String
    }


personDecoder : Decode.Decoder Person
personDecoder =
    Decode.succeed Person
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "firstName" Decode.string
        |> Pipeline.required "lastName" Decode.string


type alias PersonList =
    List Person


personList : Decode.Decoder PersonList
personList =
    Decode.list personDecoder


type alias Record =
    { id : String
    , personId : String
    , timestamp : String
    , amount : Float
    , note : String
    }


recordDecoder : Decode.Decoder Record
recordDecoder =
    Decode.succeed Record
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "personId" Decode.string
        |> Pipeline.required "timestamp" Decode.string
        |> Pipeline.required "amount" Decode.float
        |> Pipeline.required "note" Decode.string


type alias RecordList =
    List Record


recordListDecoder : Decode.Decoder RecordList
recordListDecoder =
    Decode.list recordDecoder


type alias RecordForm =
    { personId : String
    , timestamp : String
    , amount : Float
    , note : String
    }


recordFormEncoder : RecordForm -> Encode.Value
recordFormEncoder record =
    Encode.object
        [ ( "personId", Encode.string record.personId )
        , ( "timestamp", Encode.string (record.timestamp ++ ":00Z") )
        , ( "amount", Encode.float record.amount )
        , ( "note", Encode.string record.note )
        ]


baseApiUrl : String
baseApiUrl =
    "https://ccmi.fit.cvut.cz:8889/"


emptyForm : RecordForm
emptyForm =
    { personId = "", timestamp = "", amount = 0, note = "" }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { personDropdown =
            { state = Dropdown.newState "personDropdown"
            , selectedPerson = Nothing
            }
      , persons = Loading
      , records = Loading
      , deleteResult = NotAsked
      , addResult = NotAsked
      , form = emptyForm
      }
    , Cmd.batch [ getPersons, getRecords ]
    )


getPersons : Cmd Msg
getPersons =
    Http.get
        { url = baseApiUrl ++ "people"
        , expect = Http.expectJson GetPersons personList
        }


getRecords : Cmd Msg
getRecords =
    Http.get
        { url = baseApiUrl ++ "records"
        , expect = Http.expectJson GetRecords recordListDecoder
        }


deleteRecord : String -> Cmd Msg
deleteRecord recordId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = baseApiUrl ++ "records/" ++ recordId
        , body = Http.emptyBody
        , expect = Http.expectWhatever RecordDeleted
        , timeout = Nothing
        , tracker = Nothing
        }


addRecord : RecordForm -> Cmd Msg
addRecord form =
    Http.post
        { url = baseApiUrl ++ "records"
        , body = Http.jsonBody <| recordFormEncoder form
        , expect = Http.expectWhatever RecordAdded
        }



-- Dropdown config


personDropdownConfig : Dropdown.Config Msg Person
personDropdownConfig =
    Dropdown.newConfig SelectedPerson personToDropdownLabel
        |> Dropdown.withItemClass "border-bottom border-silver p1 gray"
        |> Dropdown.withMenuClass "border border-gray"
        |> Dropdown.withMenuStyles [ ( "background", "white" ) ]
        |> Dropdown.withPrompt "Select ..."
        |> Dropdown.withPromptClass "gray"
        |> Dropdown.withSelectedClass "bold"
        |> Dropdown.withSelectedStyles [ ( "color", "black" ) ]
        |> Dropdown.withTriggerClass "col-12 bg-white p1"
        |> Dropdown.withTriggerStyles [ ( "border", "solid #767676" ) ]


personToDropdownLabel : Person -> String
personToDropdownLabel { firstName, lastName } =
    String.join " " [ firstName, lastName ]



-- Dropdown required Messages


type Msg
    = SelectedPerson (Maybe Person)
    | DropdownMsg (Dropdown.Msg Person)
    | GetPersons (Result Http.Error PersonList)
    | GetRecords (Result Http.Error RecordList)
    | DeleteRecord String
    | RecordDeleted (Result Http.Error ())
    | RecordAdded (Result Http.Error ())
    | FormAmount String
    | FormTimestamp String
    | FormNote String
    | SubmitForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ personDropdown, form } as model) =
    case msg of
        DropdownMsg dropdownMsg ->
            let
                ( updatedDropdownState, dropdownCmd ) =
                    Dropdown.update personDropdownConfig dropdownMsg personDropdown.state

                updatedPersonDropdown =
                    { personDropdown | state = updatedDropdownState }
            in
            ( { model | personDropdown = updatedPersonDropdown }
            , dropdownCmd
            )

        SelectedPerson selectedPerson ->
            let
                updatedPersonDropdown =
                    { personDropdown | selectedPerson = selectedPerson }
            in
            ( { model | personDropdown = updatedPersonDropdown }
            , Cmd.none
            )

        GetPersons result ->
            case result of
                Ok persons ->
                    ( { model | persons = Success <| List.sortBy (\x -> x.firstName ++ x.lastName) persons }, Cmd.none )

                Err err ->
                    ( { model | persons = Failure err }, Cmd.none )

        GetRecords result ->
            case result of
                Ok records ->
                    ( { model | records = Success records }, Cmd.none )

                Err err ->
                    ( { model | records = Failure err }, Cmd.none )

        DeleteRecord recordId ->
            ( { model | deleteResult = Loading }, deleteRecord recordId )

        RecordDeleted result ->
            case result of
                Ok _ ->
                    ( { model | deleteResult = Success () }, getRecords )

                Err err ->
                    ( { model | deleteResult = Failure err }, Cmd.none )

        FormAmount amountStr ->
            let
                amount =
                    amountStr
                        |> String.toFloat
                        |> Maybe.withDefault 0
            in
            ( { model | form = { form | amount = amount } }, Cmd.none )

        FormTimestamp timestamp ->
            ( { model | form = { form | timestamp = timestamp } }, Cmd.none )

        FormNote note ->
            ( { model | form = { form | note = note } }, Cmd.none )

        SubmitForm ->
            case personDropdown.selectedPerson of
                Maybe.Just person ->
                    ( { model | addResult = Loading }, addRecord { form | personId = person.id } )

                Maybe.Nothing ->
                    -- Validating just select, rest is done by html5 validations
                    ( { model | addResult = Failure (Http.BadBody "Select person") }, Cmd.none )

        RecordAdded result ->
            case result of
                Ok _ ->
                    ( { model | addResult = Success (), form = emptyForm }, getRecords )

                Err err ->
                    ( { model | addResult = Failure err }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ linkBootstrapForDropdown
        , Html.h1 [] [ Html.text "Debt diary" ]
        , viewContent model
        ]



-- Linked bootstrap CSS


linkBootstrapForDropdown : Html.Html msg
linkBootstrapForDropdown =
    Html.node "link"
        [ Attributes.attribute "rel" "stylesheet"
        , Attributes.attribute "href" "https://cdn.jsdelivr.net/npm/bootstrap@4.3.1/dist/css/bootstrap.min.css"
        , Attributes.attribute "integrity" "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
        , Attributes.attribute "crossorigin" "anonymous"
        ]
        []


viewContent : Model -> Html.Html Msg
viewContent model =
    let
        records =
            case model.records of
                NotAsked ->
                    NotAsked

                Loading ->
                    Loading

                Failure err ->
                    Failure err

                Success data ->
                    case model.personDropdown.selectedPerson of
                        Maybe.Just person ->
                            Success ( person, List.filter (\x -> x.personId == person.id) data )

                        Maybe.Nothing ->
                            NotAsked
    in
    Html.div Styles.content
        [ personListView model.persons
        , personDetailView records model.deleteResult
        , addDebtView model
        ]


addDebtView : Model -> Html.Html Msg
addDebtView { personDropdown, persons, form, addResult } =
    let
        personDropdownOptions =
            case persons of
                Success data ->
                    data

                _ ->
                    []

        errorMessage =
            case addResult of
                Failure err ->
                    case err of
                        Http.BadBody msg ->
                            msg

                        _ ->
                            "Error happend"

                _ ->
                    ""
    in
    Html.form (Styles.addDebt ++ [ onCustomSubmit SubmitForm ])
        [ Html.div Styles.dropdown
            [ Html.label Styles.label [ Html.text "Select person" ]
            , Html.map DropdownMsg (Dropdown.view personDropdownConfig personDropdown.state personDropdownOptions personDropdown.selectedPerson)
            ]
        , Html.div []
            [ Html.label Styles.label [ Html.text "Amount" ]
            , Html.input
                (Styles.input
                    ++ [ Attributes.type_ "number"
                       , Attributes.required True
                       , Events.onInput FormAmount
                       , Attributes.value
                            (if form.amount == 0 then
                                ""

                             else
                                form.amount |> String.fromFloat
                            )
                       ]
                )
                []
            ]
        , Html.div []
            [ Html.label Styles.label [ Html.text "Timestamp" ]
            , Html.input (Styles.input ++ [ Attributes.required True, Attributes.type_ "datetime-local", Attributes.value form.timestamp, Events.onInput FormTimestamp ]) []
            ]
        , Html.div []
            [ Html.label Styles.label [ Html.text "Note" ]
            , Html.textarea (Styles.textarea ++ [ Attributes.required True, Attributes.value form.note, Events.onInput FormNote ]) []
            ]
        , Html.button [ Attributes.type_ "submit", Attributes.disabled (addResult == Loading) ]
            [ Html.text "Submit" ]
        , Html.div [ Attributes.style "color" "red" ] [ Html.text errorMessage ]
        ]


onCustomSubmit : msg -> Html.Attribute msg
onCustomSubmit msg =
    Events.custom "submit"
        (Decode.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )


personListView : WebData PersonList -> Html.Html Msg
personListView personsResult =
    Html.div Styles.personList
        (case personsResult of
            NotAsked ->
                []

            Loading ->
                [ Html.text "Loading..." ]

            Failure _ ->
                [ Html.text "Error happend" ]

            Success data ->
                data |> List.map personView
        )


personView : Person -> Html.Html Msg
personView person =
    Html.div Styles.person
        [ Html.a (Styles.personLink ++ [ Events.onClick <| SelectedPerson <| Maybe.Just person ]) [ Html.text (person.firstName ++ " " ++ person.lastName) ]
        ]


personDetailView : WebData ( Person, RecordList ) -> WebData () -> Html.Html Msg
personDetailView recordsResult deleteResult =
    Html.div Styles.personDetail
        [ Html.h2 []
            [ Html.text "Person detail" ]
        , case recordsResult of
            NotAsked ->
                Html.text "Select person"

            Loading ->
                Html.text "Loading..."

            Failure _ ->
                Html.text "Error happend"

            Success ( person, records ) ->
                let
                    totalAmount =
                        records |> List.map (\x -> x.amount) |> List.sum
                in
                Html.div []
                    ([ Html.div []
                        [ Html.strong [] [ Html.text "name: " ]
                        , Html.span [] [ Html.text (person.firstName ++ " " ++ person.lastName) ]
                        ]
                     , Html.div []
                        [ Html.strong [] [ Html.text "total sum: " ]
                        , Html.span [] [ Html.text (String.fromFloat totalAmount) ]
                        ]
                     , Html.br [] []
                     ]
                        ++ (if List.isEmpty records then
                                [ Html.text "No records found" ]

                            else
                                records |> List.map (recordDetailView deleteResult)
                           )
                    )
        ]


recordDetailView : WebData () -> Record -> Html.Html Msg
recordDetailView deleteResult record =
    Html.div []
        [ Html.div []
            [ Html.span [] [ Html.text "note: " ]
            , Html.span [] [ Html.text record.note ]
            ]
        , Html.div []
            [ Html.span [] [ Html.text "amount: " ]
            , Html.span [] [ Html.text <| String.fromFloat record.amount ]
            ]
        , Html.div []
            [ Html.span [] [ Html.text "timestamp: " ]
            , Html.span [] [ Html.text record.timestamp ]
            ]
        , Html.button [ Events.onClick (DeleteRecord record.id), Attributes.disabled (deleteResult == Loading) ] [ Html.text "Delete" ]
        , Html.hr [] []
        ]
