module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Game
import Html exposing (Html, a, button, div, form, h1, h2, h3, img, input, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onSubmit)
import Http
import Loading exposing (LoaderType(..), defaultConfig)
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
    | GameInProgress Game.Game Game.Team


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
    | GotGame (Result Http.Error Game.Game)


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

        GotGame (Ok game) ->
            case model.page of
                GameInProgress _ t ->
                    ( { model | page = GameInProgress game t }, Cmd.none )

                GameLoading _ ->
                    ( { model | page = GameInProgress game Game.NoTeam }, Cmd.none )

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

        GameInProgress game team ->
            viewGameInProgress model.playerId game team


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


viewGameInProgress : String -> Game.Game -> Game.Team -> Browser.Document Msg
viewGameInProgress playerId g team =
    { title = "Codenames Green"
    , body =
        [ viewHeader
        , div [ Attr.id "game" ]
            [ div [ Attr.id "board" ]
                (List.map
                    (\w -> div [ Attr.class "cell" ] [ text w ])
                    g.words
                )
            , div [ Attr.id "sidebar" ] (viewSidebar playerId g team)
            ]
        ]
    }


viewSidebar : String -> Game.Game -> Game.Team -> List (Html Msg)
viewSidebar playerId g team =
    [ viewJoinATeam (Game.playersOnTeam g Game.A) (Game.playersOnTeam g Game.B) ]


viewJoinATeam : Int -> Int -> Html Msg
viewJoinATeam a b =
    div [ Attr.id "join-a-team" ]
        [ h3 [] [ text "Pick a side" ]
        , p [] [ text "Pick a side to start playing. Each side has a different key card." ]
        , div [ Attr.class "buttons" ]
            [ button []
                [ span [ Attr.class "call-to-action" ] [ text "Side A" ]
                , span [ Attr.class "details" ] [ text (String.fromInt a), text " players" ]
                ]
            , button []
                [ span [ Attr.class "call-to-action" ] [ text "Side B" ]
                , span [ Attr.class "details" ] [ text (String.fromInt b), text " players" ]
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
