module Main exposing (main)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type Msg
    = SwitchIntroduction
    | SwitchPlot
    | SwitchConclution


type ActiveTab
    = Introduction
    | Plot
    | Conclusion


type alias Model =
    { activeTab : ActiveTab }


init : Model
init =
    { activeTab = Introduction }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SwitchIntroduction ->
            { model | activeTab = Introduction }

        SwitchPlot ->
            { model | activeTab = Plot }

        SwitchConclution ->
            { model | activeTab = Conclusion }


view : Model -> Html Msg
view model =
    Html.div pageStyle
        [ Html.h1 [] [ Html.text "Sunny Philadelphia" ]
        , Html.p subheadlineStyle
            [ Html.text "A short story generated by "
            , Html.a [ Attributes.href "https://www.plot-generator.org.uk/story/", Attributes.target "_blank" ] [ Html.text "Plot Generator" ]
            , Html.text "."
            ]
        , Html.div tabsStyle
            [ Html.a
                (tabStyleActiveConditional
                    (model.activeTab == Introduction)
                    ++ tabStyle
                    ++ [ Events.onClick SwitchIntroduction ]
                )
                [ Html.text "Introduction" ]
            , Html.a
                (tabStyleActiveConditional
                    (model.activeTab == Plot)
                    ++ tabStyle
                    ++ [ Events.onClick SwitchPlot ]
                )
                [ Html.text "Plot" ]
            , Html.a
                (tabStyleActiveConditional
                    (model.activeTab == Conclusion)
                    ++ tabStyle
                    ++ [ Events.onClick SwitchConclution ]
                )
                [ Html.text "Conclusion" ]
            ]
        , Html.div []
            [ case model.activeTab of
                Introduction ->
                    introductionContent

                Plot ->
                    plotContent

                Conclusion ->
                    conclusionContent
            ]
        ]


introductionContent : Html Msg
introductionContent =
    Html.div []
        [ Html.p [] [ Html.text "Jenny Giantbulb had always loved sunny Philadelphia with its frightened, fragile fields. It was a place where she felt relaxed." ]
        , Html.p [] [ Html.text "She was a noble, predatory, tea drinker with red lips and fragile feet. Her friends saw her as a hungry, hilarious hero. Once, she had even helped a puny kitten recover from a flying accident. That's the sort of woman he was." ]
        ]


plotContent : Html Msg
plotContent =
    Html.div []
        [ Html.p [] [ Html.text "Jenny walked over to the window and reflected on her idyllic surroundings. The sun shone like walking horses." ]
        , Html.p [] [ Html.text "Then she saw something in the distance, or rather someone. It was the figure of John Thunder. John was an optimistic giant with beautiful lips and short feet." ]
        , Html.p [] [ Html.text "Jenny gulped. She was not prepared for John." ]
        , Html.p [] [ Html.text "As Jenny stepped outside and John came closer, she could see the super glint in his eye." ]
        , Html.p [] [ Html.text "John gazed with the affection of 9175 spiteful broad bears. He said, in hushed tones, \"I love you and I want justice.\"" ]
        , Html.p [] [ Html.text "Jenny looked back, even more cross and still fingering the squidgy newspaper. \"John, hands up or I'll shoot,\" she replied." ]
        ]


conclusionContent : Html Msg
conclusionContent =
    Html.div []
        [ Html.p [] [ Html.text "They looked at each other with surprised feelings, like two faffdorking, faithful frogs cooking at a very cowardly Halloween party, which had piano music playing in the background and two stingy uncles bouncing to the beat." ]
        , Html.p [] [ Html.text "Jenny regarded John's beautiful lips and short feet. \"I feel the same way!\" revealed Jenny with a delighted grin." ]
        , Html.p [] [ Html.text "John looked irritable, his emotions blushing like a sleepy, squashed sausage." ]
        , Html.p [] [ Html.text "Then John came inside for a nice cup of tea." ]
        ]


pageStyle : List (Attribute Msg)
pageStyle =
    [ Attributes.style "width" "45rem"
    , Attributes.style "margin" "auto"
    ]


subheadlineStyle : List (Attribute Msg)
subheadlineStyle =
    [ Attributes.style "font-style" "italic"
    ]


tabsStyle : List (Attribute Msg)
tabsStyle =
    [ Attributes.style "display" "flex"
    , Attributes.style "width" "100%"
    , Attributes.style "border-bottom" "1px solid #000"
    ]


tabStyle : List (Attribute Msg)
tabStyle =
    [ Attributes.style "flex-grow" "1"
    , Attributes.style "text-align" "center"
    , Attributes.style "cursor" "pointer"
    , Attributes.style "padding" "0.5rem"
    , Attributes.style "color" "blue"
    ]


tabStyleActive : List (Attribute Msg)
tabStyleActive =
    [ Attributes.style "font-weight" "bold"
    , Attributes.style "color" "black"
    ]


tabStyleActiveConditional : Bool -> List (Attribute Msg)
tabStyleActiveConditional condition =
    if condition then
        tabStyleActive

    else
        []
