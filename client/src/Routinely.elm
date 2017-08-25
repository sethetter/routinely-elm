module Routinely exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { actions : List Action
    , actionLogs : List ActionLog
    , theTime : Time
    }


type alias Action =
    { id : Int
    , name : String
    , value : Int
    }


type alias ActionLog =
    { id : Int
    , name : String
    , value : Int
    , actionId : Int
    , confirmedAt : Time
    , createdAt : Time
    }

init : ( Model, Cmd Msg )
init =
    ( { actions = initActions
      , actionLogs = []
      , theTime = 0.0
      }
    , Task.perform CurrentTime Time.now
    )

initActions : List Action
initActions =
    [ { id = 1
      , name = "Brush yo teef"
      , value = 1
      }
    , { id = 2
      , name = "Clean yo rooom"
      , value = 3
      }
    ]


initActionLogs : Time -> List ActionLog
initActionLogs currentTime =
    [ { id = 1
      , name = "Brush yo teef"
      , value = 1
      , actionId = 1
      , confirmedAt = currentTime
      , createdAt = currentTime - Time.hour
      }
    , { id = 2
      , name = "Clean yo rooom"
      , value = 3
      , actionId = 2
      , confirmedAt = currentTime
      , createdAt = currentTime - Time.hour
      }
    ]


type Msg
    = NoOp
    | CurrentTime Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! [ Cmd.none ]
        CurrentTime time ->
            { model | actionLogs = initActionLogs time } ! [ Cmd.none ]


view : Model -> Html Msg
view model =
    div
        [ class "container"
        ]
        [ h1 [] [ text "Routinely!" ]
        , table
            [ class "table table-bordered" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Col 1 Header" ] ]
                ]
            ]
        ]
