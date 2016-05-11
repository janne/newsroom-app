import Html exposing (..)
import Html.App
import Http
import Task

main =
  Html.App.programWithFlags
    { init = init, subscriptions = subscriptions, update = update, view = view }

-- MODEL

type alias Model =
  {
    key : String,
    result : String
  }

type alias Flags =
  {
    key : String
  }

init : Flags -> (Model, Cmd Msg)
init flags =
  (Model flags.key "Loading...", getList flags.key)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- UPDATE

type Msg
  = FetchFail Http.Error
  | FetchSucceed String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchFail error ->
      ({ model | result = toString error }, Cmd.none)

    FetchSucceed body ->
      ({ model | result = body }, Cmd.none)

-- HTTP

getList : String -> Cmd Msg
getList key =
  let
    url = "http://www.mynewsdesk.com/services/pressroom/list/" ++ key ++ "?format=json"
  in
    Task.perform FetchFail FetchSucceed (Http.getString url)

-- VIEW

view model =
  h1 [] [ text model.result ]
