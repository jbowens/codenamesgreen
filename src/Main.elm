module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, h2, img, input, p, text)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Loading exposing (LoaderType(..), defaultConfig)
import Url
import Url.Builder as UrlBuilder
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, string, top)
import Url.Parser.Query as Query



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | Home String
    | GameLoading String
    | GameInProgress Game


type alias Game =
    { words : List String
    , oneLayout : List String
    , twoLayout : List String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    stepUrl url { key = key, page = Home "" }



---- UPDATE ----


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | IdChanged String
    | SubmitNewGame
    | GotGame (Result Http.Error Game)


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
                GameInProgress _ ->
                    ( { model | page = GameInProgress game }, Cmd.none )

                GameLoading _ ->
                    ( { model | page = GameInProgress game }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
    ( { model | page = GameLoading id }, maybeMakeGame id )


maybeMakeGame : String -> Cmd Msg
maybeMakeGame id =
    Http.post
        { url = "http://localhost:8080/new-game"
        , body = Http.jsonBody (Enc.object [ ( "game_id", Enc.string id ) ])
        , expect = Http.expectJson GotGame decodeGame
        }


decodeGame : Dec.Decoder Game
decodeGame =
    Dec.map3 Game
        (Dec.field "words" (Dec.list Dec.string))
        (Dec.field "one_layout" (Dec.list Dec.string))
        (Dec.field "two_layout" (Dec.list Dec.string))


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


viewGameInProgress : Game -> Browser.Document Msg
viewGameInProgress g =
    { title = "Codenames Green"
    , body =
        [ viewHeader
        , div [ Attr.id "game" ]
            [ div [ Attr.id "board" ]
                (List.map
                    (\w -> div [ Attr.class "cell" ] [ text w ])
                    g.words
                )
            ]
        ]
    }


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


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
