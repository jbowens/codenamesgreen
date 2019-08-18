module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Dict
import Game
import Html exposing (Html, a, button, div, form, h1, h2, h3, img, input, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Loading exposing (LoaderType(..), defaultConfig)
import Team
import Url
import Url.Builder as UrlBuilder
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, string, top)
import Url.Parser.Query as Query



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , playerId : String
    , page : Page
    }


type Page
    = NotFound
    | Home String
    | GameLoading String
    | GameInProgress Game.Model


init : String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init playerId url key =
    stepUrl url { key = key, playerId = playerId, page = Home "" }



---- UPDATE ----


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | IdChanged String
    | SubmitNewGame
    | PickTeam Team.Team
    | GameUpdate Game.Msg
    | GotGame (Result Http.Error Game.GameData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            stepUrl url model

        IdChanged id ->
            case model.page of
                Home _ ->
                    ( { model | page = Home id }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitNewGame ->
            case model.page of
                Home id ->
                    ( model, Nav.pushUrl model.key (UrlBuilder.relative [ id ] []) )

                _ ->
                    ( model, Cmd.none )

        GameUpdate gameMsg ->
            case model.page of
                GameInProgress game ->
                    let
                        ( newGame, gameCmd ) =
                            Game.update gameMsg game
                    in
                    ( { model | page = GameInProgress newGame }, Cmd.map GameUpdate gameCmd )

                _ ->
                    ( model, Cmd.none )

        GotGame (Ok data) ->
            case model.page of
                GameInProgress old ->
                    ( { model | page = GameInProgress (Game.init old.id data model.playerId) }, Cmd.none )

                GameLoading id ->
                    ( { model | page = GameInProgress (Game.init id data model.playerId) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PickTeam team ->
            case model.page of
                GameInProgress game ->
                    let
                        old =
                            game.player
                    in
                    ( { model | page = GameInProgress { game | player = { old | team = team } } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        -- TODO: display an error message
        GotGame (Err e) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    case Maybe.withDefault NullRoute (Parser.parse route url) of
        NullRoute ->
            ( { model | page = NotFound }, Cmd.none )

        Index ->
            ( { model | page = Home "" }, Cmd.none )

        GameView id ->
            stepGameView model id


stepGameView : Model -> String -> ( Model, Cmd Msg )
stepGameView model id =
    ( { model | page = GameLoading id }, Game.maybeMakeGame id GotGame )


type Route
    = NullRoute
    | Index
    | GameView String


route : Parser (Route -> a) a
route =
    oneOf
        [ map Index top
        , map GameView string
        ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            viewNotFound

        Home id ->
            viewHome id

        GameLoading id ->
            { title = "Codenames Green"
            , body = viewGameLoading id
            }

        GameInProgress game ->
            viewGameInProgress game


viewNotFound : Browser.Document Msg
viewNotFound =
    { title = "Codenames Green | Page not found"
    , body =
        [ viewHeader
        , div [ Attr.id "not-found" ]
            [ h2 [] [ text "Page not found" ]
            , p []
                [ text "That page doesn't exist. "
                , a [ Attr.href "/" ] [ text "Go to the homepage" ]
                ]
            ]
        ]
    }


viewGameInProgress : Game.Model -> Browser.Document Msg
viewGameInProgress g =
    { title = "Codenames Green"
    , body =
        [ viewHeader
        , div [ Attr.id "game" ]
            [ Html.map GameUpdate (Game.viewBoard g)
            , div [ Attr.id "sidebar" ] (viewSidebar g)
            ]
        ]
    }


viewSidebar : Game.Model -> List (Html Msg)
viewSidebar g =
    let
        teams =
            Dict.values g.players

        playersOnTeamA =
            teams
                |> List.filter (\x -> x == Team.A)
                |> List.length

        playersOnTeamB =
            teams
                |> List.filter (\x -> x == Team.B)
                |> List.length
    in
    if g.player.team == Team.None then
        [ viewJoinATeam playersOnTeamA playersOnTeamB ]

    else
        viewTeamSidebar g


viewTeamSidebar : Game.Model -> List (Html Msg)
viewTeamSidebar g =
    [ Game.viewStatus g
    , Game.viewKeycard g g.player.team
    , Html.map GameUpdate (Game.viewEventLog g)
    ]


viewJoinATeam : Int -> Int -> Html Msg
viewJoinATeam a b =
    div [ Attr.id "join-a-team" ]
        [ h3 [] [ text "Pick a side" ]
        , p [] [ text "Pick a side to start playing. Each side has a different key card." ]
        , div [ Attr.class "buttons" ]
            [ button [ onClick (PickTeam Team.A) ]
                [ span [ Attr.class "call-to-action" ] [ text "A" ]
                , span [ Attr.class "details" ] [ text "(", text (String.fromInt a), text " players)" ]
                ]
            , button [ onClick (PickTeam Team.B) ]
                [ span [ Attr.class "call-to-action" ] [ text "B" ]
                , span [ Attr.class "details" ] [ text "(", text (String.fromInt b), text " players)" ]
                ]
            ]
        ]


viewGameLoading : String -> List (Html Msg)
viewGameLoading id =
    [ viewHeader
    , div [ Attr.id "game-loading" ]
        [ Loading.render Circle { defaultConfig | size = 100, color = "#b7ec8a" } Loading.On
        ]
    ]


viewHome : String -> Browser.Document Msg
viewHome id =
    { title = "Codenames Green"
    , body =
        [ div [ Attr.id "home" ]
            [ h1 [] [ text "Codenames Green" ]
            , p [] [ text "Play cooperative Codenames online across multiple devices on a shared board. To create a new game or join an existing game, enter a game identifier and click 'GO'." ]
            , form
                [ Attr.id "new-game"
                , onSubmit SubmitNewGame
                ]
                [ input
                    [ Attr.id "game-id"
                    , Attr.name "game-id"
                    , Attr.value id
                    , onInput IdChanged
                    ]
                    []
                , button [] [ text "Go" ]
                ]
            ]
        ]
    }


viewHeader : Html Msg
viewHeader =
    div [ Attr.id "header" ] [ a [ Attr.href "/" ] [ h1 [] [ text "Codenames Green" ] ] ]



---- PROGRAM ----


main : Program String Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
