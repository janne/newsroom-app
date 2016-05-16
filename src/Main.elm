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
    , typeOfMaterial : String
    , materials : List Material
    }


type alias Material =
    { header : String
    , publishedAt : String
    }


type alias Flags =
    { key : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { key = flags.key
            , typeOfMaterial = "pressrelease"
            , materials = []
            }
    in
        ( model, getList model )



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


getList : Model -> Cmd Msg
getList model =
    let
        url =
            "http://www.mynewsdesk.com/services/pressroom/list/"
                ++ model.key
                ++ "?type_of_media="
                ++ model.typeOfMaterial
                ++ "&format=json"
    in
        Task.perform FetchFail FetchSucceed (Http.get decodeMaterials url)


decodeMaterial : Json.Decoder Material
decodeMaterial =
    Json.object2 Material
        ("header" := Json.string)
        ("published_at" := Json.string)


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
        [ td [] [ text material.header ]
        , td [] [ text material.publishedAt ]
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
