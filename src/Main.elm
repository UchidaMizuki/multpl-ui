module Main exposing (..)

import Browser
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game exposing (Hand, Play, Played)
import Html exposing (Html)
import Html.Attributes exposing (selected)
import List.Nonempty as Nonempty exposing (Nonempty)
import Process
import Random
import Task
import Tuple exposing (first, second)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { ranks : Nonempty Int
    , lengthRanks : Int
    , hand : Hand
    , handOpponent : Hand
    , field : Played
    , selected : Played
    , turn : Player
    , winner : Maybe Player
    , color :
        { dark : Element.Color
        , light : Element.Color
        , board : Element.Color
        , button : Element.Color
        }
    , font :
        { serif : Font.Font
        , sansSerif : Font.Font
        }
    }


type Player
    = You
    | Opponent


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ranks : Nonempty Int
        ranks =
            Nonempty.Nonempty 1 <|
                List.range 2 7

        -- 13
        model : Model
        model =
            { ranks = ranks
            , lengthRanks = Nonempty.length ranks
            , hand = Dict.empty
            , handOpponent = Dict.empty
            , field =
                { first = Nothing
                , second = Nothing
                }
            , selected =
                { first = Nothing
                , second = Nothing
                }
            , turn = You
            , winner = Nothing

            -- https://colorhunt.co/palette/eb455ffcffe7bad7e92b3467
            , color =
                { dark = Element.rgb255 43 52 103
                , light = Element.rgb255 254 255 247
                , board = Element.rgb255 138 161 190
                , button = Element.rgb255 208 66 58
                }
            , font =
                { serif =
                    Font.external
                        { name = "Crimson Text"
                        , url = "https://fonts.googleapis.com/css?family=Crimson Text"
                        }
                , sansSerif =
                    Font.external
                        { name = "Poppins"
                        , url = "https://fonts.googleapis.com/css?family=Poppins"
                        }
                }
            }
    in
    ( model
    , Cmd.batch
        [ Nonempty.sample model.ranks
            |> Random.generate (\rank -> PlayCardInit rank)
        , Nonempty.sample model.ranks
            |> Random.list 6
            |> Random.generate DrawCardsInit
        , Nonempty.sample model.ranks
            |> Random.list 6
            |> Random.generate DrawCardsOpponent
        ]
    )



-- UPDATE


type Msg
    = PlayCardInit Int
    | DrawCardsInit (List Int)
    | DrawCards (List Int)
    | DrawCardsOpponent (List Int)
    | SelectCard Int
    | RestoreCardFirst Int
    | RestoreCardSecond Int
    | PressPass
    | PressPlay
    | PlayCardsOpponent (Maybe Play)
    | TurnOpponent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        field : Played
        field =
            model.field

        selected : Played
        selected =
            model.selected

        addHand : Int -> Hand -> Hand
        addHand rank hand =
            Dict.update rank
                (\maybeNumber ->
                    case maybeNumber of
                        Just number ->
                            Just <| number + 1

                        Nothing ->
                            Just 1
                )
                hand

        clearHand : Hand -> Hand
        clearHand =
            Dict.filter <|
                \_ number ->
                    number > 0

        takeHand : Int -> Hand -> Hand
        takeHand rank hand =
            Dict.update rank
                (\maybeNumber ->
                    case maybeNumber of
                        Just number ->
                            Just <| number - 1

                        Nothing ->
                            Just 0
                )
                hand

        fieldNew : Int -> Int -> Played
        fieldNew first second =
            if first <= second then
                { first = Just first
                , second = Just second
                }

            else
                { first = Just second
                , second = Just first
                }

        turnOpponent : Cmd Msg
        turnOpponent =
            Process.sleep 5.0e2
                |> Task.perform (\_ -> TurnOpponent)
    in
    case msg of
        PlayCardInit rank ->
            ( { model
                | field =
                    { field | first = Just rank }
              }
            , Cmd.none
            )

        DrawCardsInit ranks ->
            ( { model
                | hand =
                    ranks
                        |> List.foldl addHand model.hand
              }
            , Cmd.none
            )

        DrawCards ranks ->
            ( { model
                | hand =
                    ranks
                        |> List.foldl addHand model.hand
                , turn = Opponent
              }
            , turnOpponent
            )

        DrawCardsOpponent ranks ->
            ( { model
                | handOpponent =
                    ranks
                        |> List.foldl addHand model.handOpponent
                , turn = You
              }
            , Cmd.none
            )

        SelectCard rank ->
            ( case ( selected.first, selected.second ) of
                ( Just _, Just _ ) ->
                    model

                ( Just _, Nothing ) ->
                    { model
                        | hand =
                            takeHand rank model.hand
                        , selected =
                            { selected | second = Just rank }
                    }

                ( Nothing, _ ) ->
                    { model
                        | hand =
                            takeHand rank model.hand
                        , selected =
                            { selected | first = Just rank }
                    }
            , Cmd.none
            )

        RestoreCardFirst rank ->
            ( { model
                | hand =
                    addHand rank model.hand
                , selected =
                    { selected | first = Nothing }
              }
            , Cmd.none
            )

        RestoreCardSecond rank ->
            ( { model
                | hand =
                    addHand rank model.hand
                , selected =
                    { selected | second = Nothing }
              }
            , Cmd.none
            )

        PressPass ->
            ( { model
                | hand =
                    case ( selected.first, selected.second ) of
                        ( Just first, Just second ) ->
                            model.hand
                                |> addHand first
                                |> addHand second

                        ( Just first, Nothing ) ->
                            model.hand
                                |> addHand first

                        ( Nothing, Just second ) ->
                            model.hand
                                |> addHand second

                        ( Nothing, Nothing ) ->
                            model.hand
                , selected =
                    { first = Nothing
                    , second = Nothing
                    }
              }
            , Nonempty.sample model.ranks
                |> Random.list 1
                |> Random.generate DrawCards
            )

        PressPlay ->
            case ( selected.first, selected.second, field.first ) of
                ( Just selectedFirst, Just selectedSecond, Just fieldFirst ) ->
                    if modBy fieldFirst (selectedFirst + selectedSecond) == 0 then
                        let
                            hand : Hand
                            hand =
                                clearHand model.hand

                            modelNew : Model
                            modelNew =
                                { model
                                    | hand = hand
                                    , selected =
                                        { first = Nothing
                                        , second = Nothing
                                        }
                                    , field = fieldNew selectedFirst selectedSecond
                                    , turn = Opponent
                                    , winner =
                                        if Dict.isEmpty hand then
                                            Just You

                                        else
                                            Nothing
                                }
                        in
                        ( modelNew
                        , if modelNew.winner == Nothing then
                            turnOpponent

                          else
                            Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayCardsOpponent (Just play) ->
            let
                first : Int
                first =
                    play.first

                second : Int
                second =
                    play.second

                handOpponent : Hand
                handOpponent =
                    model.handOpponent
                        |> takeHand first
                        |> takeHand second
                        |> clearHand
            in
            ( { model
                | handOpponent = handOpponent
                , field = fieldNew first second
                , turn = You
                , winner =
                    if Dict.isEmpty handOpponent then
                        Just Opponent

                    else
                        Nothing
              }
            , Cmd.none
            )

        PlayCardsOpponent Nothing ->
            ( model
            , Nonempty.sample model.ranks
                |> Random.list 1
                |> Random.generate DrawCardsOpponent
            )

        TurnOpponent ->
            ( model
            , case model.field.first of
                Just fieldFirst ->
                    Game.generatorPlay model.handOpponent fieldFirst model.hand
                        |> Random.generate PlayCardsOpponent

                Nothing ->
                    Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color model.color.dark
        -- , Font.bold
        , Font.color model.color.dark
        ]
    <|
        Element.column
            [ Element.width <| Element.px 864 -- 1536
            , Element.height <| Element.px 864
            , Element.centerX
            , Element.centerY
            , Element.padding 10
            , Element.spacing 5
            , Border.rounded 20
            , Background.color model.color.board
            ]
            [ viewOpponent model
            , viewField model
            , viewYour model
            ]


viewOpponent : Model -> Element Msg
viewOpponent model =
    Element.column
        [ Element.width Element.fill
        , Element.height <| Element.fillPortion 3
        , Element.spacing 5
        ]
        [ viewOpponentHand model
        , viewOpponentMenu model
        ]


viewOpponentHand : Model -> Element Msg
viewOpponentHand model =
    let
        lengthHand : Int
        lengthHand =
            Dict.size model.handOpponent

        cards : List Card
        cards =
            Dict.toList model.handOpponent
                |> List.map CardHandOpponent
    in
    viewHand model lengthHand cards


viewOpponentMenu : Model -> Element Msg
viewOpponentMenu model =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.fillPortion 1
        , Font.family [ model.font.sansSerif ]
        , Font.size 50
        , Font.color model.color.dark
        ]
    <|
        Element.el
            [ Element.centerX
            , Element.centerY
            ]
        <|
            case ( model.winner, model.turn ) of
                ( Just You, _ ) ->
                    Element.text "You Win!"

                ( Just Opponent, _ ) ->
                    Element.text "You Lose!"

                ( Nothing, You ) ->
                    Element.text "Your Turn"

                ( Nothing, Opponent ) ->
                    Element.text "Opponent's Turn"


viewField : Model -> Element Msg
viewField model =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.fillPortion 2
        ]
        [ Element.el [ Element.width <| Element.fillPortion (model.lengthRanks - 1) ] Element.none
        , Element.el
            [ Element.width <| Element.fillPortion 3
            , Element.height Element.fill
            ]
          <|
            viewCards model
                [ CardFieldFirst model.field.first
                , CardFieldSecond model.field.second
                ]
        , Element.el [ Element.width <| Element.fillPortion (model.lengthRanks - 2) ] Element.none
        ]


viewYour : Model -> Element Msg
viewYour model =
    Element.column
        [ Element.width Element.fill
        , Element.height <| Element.fillPortion 5
        , Element.spacing 5
        ]
        [ viewYourSelected model
        , viewYourHand model
        , viewYourMenu model
        ]


viewYourSelected : Model -> Element Msg
viewYourSelected model =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.fillPortion 2
        ]
        [ Element.el [ Element.width <| Element.fillPortion (model.lengthRanks - 2) ] Element.none
        , Element.el
            [ Element.width <| Element.fillPortion 4
            , Element.height Element.fill
            ]
          <|
            viewCards model
                [ CardSelectedFirst model.selected.first
                , CardSelectedSecond model.selected.second
                ]
        , Element.el [ Element.width <| Element.fillPortion (model.lengthRanks - 2) ] Element.none
        ]


viewYourHand : Model -> Element Msg
viewYourHand model =
    let
        lengthHand : Int
        lengthHand =
            Dict.size model.hand

        cards : List Card
        cards =
            Dict.toList model.hand
                |> List.map CardHand
    in
    viewHand model lengthHand cards


viewHand : Model -> Int -> List Card -> Element Msg
viewHand model lengthHand cards =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.fillPortion 2
        ]
        [ Element.el [ Element.width <| Element.fillPortion (model.lengthRanks - lengthHand) ] Element.none
        , Element.el
            [ Element.width <| Element.fillPortion (lengthHand * 2)
            , Element.height Element.fill
            ]
          <|
            viewCards model cards
        , Element.el [ Element.width <| Element.fillPortion (model.lengthRanks - lengthHand) ] Element.none
        ]


viewYourMenu : Model -> Element Msg
viewYourMenu model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Font.family [ model.font.sansSerif ]
        , Font.size 40
        , Font.color model.color.light
        ]
    <|
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.el [ Element.width <| Element.fillPortion (model.lengthRanks - 7) ] Element.none
            , Input.button
                [ Element.width <| Element.fillPortion 6
                , Element.height Element.fill
                , Border.rounded 10
                , Background.color model.color.button
                ]
                { onPress =
                    if model.turn == You && model.winner == Nothing then
                        Just PressPass

                    else
                        Nothing
                , label =
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                    <|
                        Element.text "Draw 1 Card"
                }
            , Element.el [ Element.width <| Element.fillPortion 2 ] Element.none
            , Input.button
                [ Element.width <| Element.fillPortion 6
                , Element.height Element.fill
                , Border.rounded 10
                , Background.color model.color.button
                ]
                { onPress =
                    if model.turn == You && model.winner == Nothing then
                        Just PressPlay

                    else
                        Nothing
                , label =
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                    <|
                        Element.text "Play 2 Cards"
                }
            , Element.el [ Element.width <| Element.fillPortion (model.lengthRanks - 7) ] Element.none
            ]


type Card
    = CardFieldFirst (Maybe Int)
    | CardFieldSecond (Maybe Int)
    | CardSelectedFirst (Maybe Int)
    | CardSelectedSecond (Maybe Int)
    | CardHand ( Int, Int )
    | CardHandOpponent ( Int, Int )


viewCards : Model -> List Card -> Element Msg
viewCards model cards =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 5
        ]
    <|
        List.map (viewCard model) cards


viewCard : Model -> Card -> Element Msg
viewCard model card =
    case card of
        CardFieldFirst (Just rank) ->
            viewCardJust model rank 1 2

        CardFieldFirst Nothing ->
            viewCardNothing model 2

        CardFieldSecond (Just rank) ->
            viewCardJust model rank 1 1

        CardFieldSecond Nothing ->
            viewCardNothing model 1

        CardSelectedFirst (Just rank) ->
            Input.button
                [ Element.width Element.fill
                , Element.height Element.fill
                , Border.rounded 10
                ]
                { onPress =
                    if model.turn == You && model.winner == Nothing then
                        Just <| RestoreCardFirst rank

                    else
                        Nothing
                , label = viewCardJust model rank 1 1
                }

        CardSelectedFirst Nothing ->
            viewCardNothing model 1

        CardSelectedSecond (Just rank) ->
            Input.button
                [ Element.width Element.fill
                , Element.height Element.fill
                , Border.rounded 10
                ]
                { onPress =
                    if model.turn == You && model.winner == Nothing then
                        Just <| RestoreCardSecond rank

                    else
                        Nothing
                , label = viewCardJust model rank 1 1
                }

        CardSelectedSecond Nothing ->
            viewCardNothing model 1

        CardHand ( rank, number ) ->
            if number == 0 then
                viewCardNothing model 1

            else
                Input.button
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Border.rounded 10
                    ]
                    { onPress =
                        if model.turn == You && model.winner == Nothing then
                            Just <| SelectCard rank

                        else
                            Nothing
                    , label = viewCardJust model rank number 1
                    }

        CardHandOpponent ( rank, number ) ->
            viewCardJust model rank number 1


viewCardJust : Model -> Int -> Int -> Int -> Element Msg
viewCardJust model rank number portion =
    Element.el
        [ Element.width <| Element.fillPortion portion
        , Element.height Element.fill
        , Element.padding 5
        , Border.rounded 10
        , Background.color model.color.light
        , Font.family [ model.font.serif ]
        , Font.size 60
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ viewCardJustRank rank
            , viewCardJustNumber number
            ]


viewCardNothing : Model -> Int -> Element Msg
viewCardNothing model portion =
    Element.el
        [ Element.width <| Element.fillPortion portion
        , Element.height Element.fill
        , Border.rounded 10
        , Background.color model.color.dark
        ]
    <|
        Element.none


viewCardJustRank : Int -> Element Msg
viewCardJustRank rank =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Element.text <|
            String.fromInt rank


viewCardJustNumber : Int -> Element Msg
viewCardJustNumber number =
    if number == 1 then
        Element.none

    else
        Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
        <|
            Element.el
                [ Element.alignRight
                , Element.alignBottom
                ]
            <|
                Element.text <|
                    ("× " ++ String.fromInt number)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
