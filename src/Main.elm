module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App
import Http
import Task
import String
import Json.Decode as Json exposing ((:=))
import Json.Encode
import VirtualDom


main =
    Html.App.programWithFlags { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { key : String
    , typeOfMaterial : String
    , materials : List Material
    , status : Status
    , material : Maybe Material
    }


type alias Material =
    { id : Int
    , header : String
    , publishedAt : String
    , imageUrl : Maybe String
    , body : Maybe String
    }


type alias Flags =
    { key : String
    }


type Status
    = Loading
    | Done
    | Failed


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { key = flags.key
      , typeOfMaterial = "pressrelease"
      , materials = []
      , status = Done
      , material = Nothing
      }
    , getList flags.key "pressrelease"
    )


statusText : Status -> String
statusText status =
    case status of
        Loading ->
            "Loading..."

        Done ->
            "Done"

        Failed ->
            "Failed"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = FetchFail Http.Error
    | FetchSucceed (List Material)
    | ChangeType String
    | ShowMaterial Material
    | ShowList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchFail error ->
            ( { model | materials = [], status = Failed }, Cmd.none )

        FetchSucceed materials ->
            ( { model | materials = materials, material = Nothing, status = Done }, Cmd.none )

        ChangeType typeOfMaterial ->
            ( { model | typeOfMaterial = typeOfMaterial, status = Loading }, getList model.key typeOfMaterial )

        ShowMaterial material ->
            ( { model | material = Just material }, Cmd.none )

        ShowList ->
            ( { model | material = Nothing }, Cmd.none )



-- HTTP


getList : String -> String -> Cmd Msg
getList key typeOfMaterial =
    let
        url =
            "http://www.mynewsdesk.com/services/pressroom/list/"
                ++ key
                ++ "?type_of_media="
                ++ typeOfMaterial
                ++ "&format=json"
                ++ "&limit=100"
    in
        Task.perform FetchFail FetchSucceed (Http.get decodeMaterials url)


decodeMaterial : Json.Decoder Material
decodeMaterial =
    Json.object5 Material
        (Json.map (String.toInt >> Result.withDefault 0) ("id" := Json.string))
        ("header" := Json.string)
        ("published_at" := Json.string)
        (Json.maybe <| "image_medium" := Json.string)
        (Json.maybe <| "body" := Json.string)


decodeMaterials : Json.Decoder (List Material)
decodeMaterials =
    Json.oneOf
        [ Json.at [ "items", "item" ] (Json.list decodeMaterial)
        , Json.succeed []
        ]



-- UTIL


(=>) =
    (,)



-- VIEW


viewNavTitle title =
    [ h5 [ class "nav-group-title" ]
        [ text title ]
    ]


viewNavItem model ( title, typeOfMaterial ) =
    span [ class "nav-group-item", onClick (ChangeType typeOfMaterial) ]
        [ text title ]


viewNav model =
    let
        items =
            viewNavTitle "Material"
                ++ List.map (viewNavItem model)
                    [ "Pressreleases" => "pressrelease", "News" => "news", "Blog Posts" => "blog_post" ]
                ++ viewNavTitle "Attachments"
                ++ List.map (viewNavItem model)
                    [ "Images" => "image", "Videos" => "video", "Documents" => "document" ]
                ++ viewNavTitle "Other"
                ++ List.map (viewNavItem model)
                    [ "Contact People" => "contact_person", "Events" => "event" ]
    in
        nav [ class "nav-group" ]
            items


viewMaterialLine material =
    tr [ onClick (ShowMaterial material) ]
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


viewImage material =
    case material.imageUrl of
        Nothing ->
            text ""

        Just url ->
            img [ src url ] []


raw : String -> Html Msg
raw html =
    span [ VirtualDom.property "innerHTML" (Json.Encode.string html) ] []


viewBody material =
    case material.body of
        Nothing ->
            text ""

        Just body ->
            div [] [ raw body ]


viewMaterial : Model -> Material -> Html Msg
viewMaterial model material =
    div [ style [ "padding" => "20px" ] ]
        [ button [ onClick ShowList ] [ text "Back to list" ]
        , h2 [] [ text material.header ]
        , div []
            [ span [] [ text model.typeOfMaterial ]
            , text " • "
            , span [] [ text material.publishedAt ]
            ]
        , viewImage material
        , viewBody material
        ]


viewMaterialOrTable : Model -> Html Msg
viewMaterialOrTable model =
    case model.material of
        Nothing ->
            viewMaterialTable model

        Just material ->
            viewMaterial model material


view model =
    div [ class "window" ]
        [ div [ class "window-content" ]
            [ div [ class "pane-group" ]
                [ div [ class "pane pane-sm sidebar" ]
                    [ viewNav model ]
                , div [ class "pane" ]
                    [ viewMaterialOrTable model ]
                ]
            ]
        , div [ class "toolbar toolbar-footer", style [ "padding-left" => "4px" ] ]
            [ text <| statusText model.status ]
        ]
