import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Http
import Task
import String
import Json.Decode as Json exposing ((:=))

main =
  Html.App.programWithFlags
    { init = init, subscriptions = subscriptions, update = update, view = view }

-- MODEL

type alias Model =
  {
    key : String,
    materials : List String
  }

type alias Flags =
  {
    key : String
  }

init : Flags -> (Model, Cmd Msg)
init flags =
  (Model flags.key [], getList flags.key)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- UPDATE

type Msg
  = FetchFail Http.Error
  | FetchSucceed (List String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchFail error ->
      (model, Cmd.none)

    FetchSucceed materials ->
      ({ model | materials = materials }, Cmd.none)

-- HTTP

getList : String -> Cmd Msg
getList key =
  let
    url = "http://www.mynewsdesk.com/services/pressroom/list/" ++ key ++ "?format=json"
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeMaterials url)

decodeMaterials : Json.Decoder (List String)
decodeMaterials =
  Json.at ["items", "item"] ( Json.list ("header" := Json.string) )


-- VIEW

view model =
  div [ class "window" ] [
    div [ class "window-content" ] [
      div [ class "pane-group" ] [
        div [ class "pane pane-sm sidebar" ] [
          nav [ class "nav-group" ] [
            h5 [ class "nav-group-title" ] [ text "Material" ],
            span [ class "nav-group-item active" ] [ text "Pressreleases" ],
            span [ class "nav-group-item" ] [ text "News" ],
            span [ class "nav-group-item" ] [ text "Blog Posts" ],
            h5 [ class "nav-group-title" ] [ text "Attachments" ],
            span [ class "nav-group-item" ] [ text "Images" ],
            span [ class "nav-group-item" ] [ text "Videos" ],
            span [ class "nav-group-item" ] [ text "Documents" ],
            h5 [ class "nav-group-title" ] [ text "Other" ],
            span [ class "nav-group-item" ] [ text "Contact People" ],
            span [ class "nav-group-item" ] [ text "Events" ]
          ]
        ],
        div [ class "pane" ] [
          table [ class "table-striped" ] [
            thead [] [
              tr [] [
                th [] [ text "Name" ],
                th [] [ text "Published at" ]
              ]
            ],
            tbody [] <| List.map (\s -> tr [] [ td [] [ text s ] ]) model.materials
          ]
        ]
      ]
    ],
    div [ class "toolbar toolbar-footer" ] [
    ]
  ]
