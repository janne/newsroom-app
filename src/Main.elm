module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Http
import Task
import String
import Json.Decode as Json exposing ((:=))


main =
    Html.App.programWithFlags { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { key : String
    , materials : List Material
    }


type alias Material =
    { header : String
    }


type alias Flags =
    { key : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { key = flags.key
      , materials = []
      }
    , getList flags.key
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = FetchFail Http.Error
    | FetchSucceed (List Material)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchFail error ->
            ( model, Cmd.none )

        FetchSucceed materials ->
            ( { model | materials = materials }, Cmd.none )



-- HTTP


getList : String -> Cmd Msg
getList key =
    let
        url =
            "http://www.mynewsdesk.com/services/pressroom/list/" ++ key ++ "?format=json"
    in
        Task.perform FetchFail FetchSucceed (Http.get decodeMaterials url)


decodeMaterial : Json.Decoder Material
decodeMaterial =
    Json.object1 Material
        ("header" := Json.string)


decodeMaterials : Json.Decoder (List Material)
decodeMaterials =
    Json.at [ "items", "item" ] (Json.list decodeMaterial)



-- VIEW


viewNavTitle title =
    [ h5 [ class "nav-group-title" ]
        [ text title ]
    ]


viewNavItem model title =
    span [ class "nav-group-item" ]
        [ text title ]


viewNav model =
    let
        items =
            viewNavTitle "Material"
                ++ List.map (viewNavItem model) [ "Pressreleases", "News", "Blog Posts" ]
                ++ viewNavTitle "Attachments"
                ++ List.map (viewNavItem model) [ "Images", "Videos", "Documents" ]
                ++ viewNavTitle "Other"
                ++ List.map (viewNavItem model) [ "Contact People", "Events" ]
    in
        nav [ class "nav-group" ]
            items


viewMaterialLine material =
    tr []
        [ td []
            [ text material.header ]
        ]


viewMaterialTable model =
    table [ class "table-striped" ]
        [ thead []
            [ tr []
                [ th []
                    [ text "Name" ]
                , th []
                    [ text "Published at" ]
                ]
            ]
        , tbody [] <| List.map viewMaterialLine model.materials
        ]


view model =
    div [ class "window" ]
        [ div [ class "window-content" ]
            [ div [ class "pane-group" ]
                [ div [ class "pane pane-sm sidebar" ]
                    [ viewNav model ]
                , div [ class "pane" ]
                    [ viewMaterialTable model ]
                ]
            ]
        , div [ class "toolbar toolbar-footer" ]
            []
        ]
