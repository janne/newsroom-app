port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App
import Http
import Task
import Dict exposing (Dict)
import String
import Json.Decode as Json exposing ((:=))
import Json.Encode
import VirtualDom


main =
    Html.App.programWithFlags { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { key : String
    , pressroom : String
    , typeOfMedia : String
    , materials : Dict String (List Material)
    , status : Status
    , material : Maybe Material
    }


type alias Material =
    { id : Int
    , typeOfMedia : String
    , header : String
    , publishedAt : String
    , imageUrl : Maybe String
    , body : Maybe String
    }


type alias Flags =
    { key : String
    , pressroom : String
    }


type Status
    = Loading
    | Done
    | Failed


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { key = flags.key
      , pressroom = flags.pressroom
      , typeOfMedia = "pressrelease"
      , materials = Dict.empty
      , status = Loading
      , material = Nothing
      }
    , getList flags.key flags.pressroom "pressrelease"
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



-- PORTS


port scrollTop : String -> Cmd msg



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
            ( { model | materials = Dict.empty, status = Failed }, Cmd.none )

        FetchSucceed items ->
            let
                newMaterials =
                    Dict.insert model.typeOfMedia items model.materials
            in
                ( { model | materials = newMaterials, material = Nothing, status = Done }
                , scrollTop "pane-content"
                )

        ChangeType typeOfMedia ->
            if model.status == Loading then
                ( model, Cmd.none )
            else
                case Dict.get typeOfMedia model.materials of
                    Nothing ->
                        ( { model | typeOfMedia = typeOfMedia, status = Loading }
                        , getList model.key model.pressroom typeOfMedia
                        )

                    Just items ->
                        ( { model | typeOfMedia = typeOfMedia, material = Nothing }
                        , scrollTop "pane-content"
                        )

        ShowMaterial material ->
            if model.status == Loading then
                ( model, Cmd.none )
            else
                ( { model | material = Just material }, scrollTop "pane-content" )

        ShowList ->
            ( { model | material = Nothing }, Cmd.none )



-- HTTP


getList : String -> String -> String -> Cmd Msg
getList key pressroom typeOfMedia =
    let
        url =
            "http://www.mynewsdesk.com/services/pressroom/list/"
                ++ key
                ++ "?type_of_media="
                ++ typeOfMedia
                ++ "&pressroom="
                ++ pressroom
                ++ "&format=json"
                ++ "&limit=100"
    in
        Task.perform FetchFail FetchSucceed (Http.get decodeItems url)


decodeItem : Json.Decoder Material
decodeItem =
    Json.object6 Material
        (Json.map (String.toInt >> Result.withDefault 0) ("id" := Json.string))
        ("type_of_media" := Json.string)
        ("header" := Json.string)
        ("published_at" := Json.string)
        (Json.maybe <| "image_medium" := Json.string)
        (Json.maybe <| "body" := Json.string)


decodeItems : Json.Decoder (List Material)
decodeItems =
    Json.oneOf
        [ Json.at [ "items", "item" ] (Json.list decodeItem)
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


viewNavItem model ( title, typeOfMedia ) =
    span [ class "nav-group-item", onClick (ChangeType typeOfMedia) ]
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
    let
        maybeItems =
            Dict.get model.typeOfMedia model.materials
    in
        table [ class "table-striped" ]
            [ thead []
                [ tr []
                    [ th []
                        [ text "Name" ]
                    , th []
                        [ text "Published at" ]
                    ]
                ]
            , tbody [] <| List.map viewMaterialLine <| Maybe.withDefault [] maybeItems
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


viewTypeTag : Material -> Html Msg
viewTypeTag material =
    div [ style [ "margin-bottom" => "4px" ] ]
        [ span
            [ style
                [ "background-color" => "#b5d156"
                , "border-radius" => "3px"
                , "color" => "#fff"
                , "display" => "inline-block"
                , "font-size" => "14px"
                , "font-weight" => "bold"
                , "padding" => "2px"
                ]
            ]
            [ text material.typeOfMedia ]
        , text " • "
        , span [] [ text material.publishedAt ]
        ]


viewMaterial : Material -> Html Msg
viewMaterial material =
    div [ style [ "padding" => "20px" ] ]
        [ button [ onClick ShowList ] [ text "Back to list" ]
        , h2 [] [ text material.header ]
        , viewTypeTag material
        , viewImage material
        , viewBody material
        ]


viewMaterialOrTable : Model -> Html Msg
viewMaterialOrTable model =
    case model.material of
        Nothing ->
            viewMaterialTable model

        Just material ->
            viewMaterial material


view model =
    div [ class "window" ]
        [ div [ class "window-content" ]
            [ div [ class "pane-group" ]
                [ div [ class "pane pane-sm sidebar" ]
                    [ viewNav model ]
                , div [ class "pane", id "pane-content" ]
                    [ viewMaterialOrTable model ]
                ]
            ]
        , div [ class "toolbar toolbar-footer", style [ "padding-left" => "4px" ] ]
            [ text <| statusText model.status ]
        ]
