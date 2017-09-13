module Routinely exposing (..)

import Date exposing (Date, Day)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List
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
    , weeklyActionLogs : List ActionLog
    , theTime : Time
    }


type alias Action =
    { id : Int
    , name : String
    , value : Int
    , perWeek : Int
    , perDay : Int
    }


type alias ActionLog =
    { id : Int
    , name : String
    , value : Int
    , actionId : Int
    , createdAt : String
    }


init : ( Model, Cmd Msg )
init =
    ( { actions = []
      , actionLogs = []
      , weeklyActionLogs = []
      , theTime = 0.0
      }
    , Task.perform CurrentTime Time.now
    )


type Msg
    = NoOp
    | CurrentTime Time
    | GetActionsResponse (Result Http.Error (List Action))
    | GetActionLogsResponse (Result Http.Error (List ActionLog))
    | CreateActionLog Action
    | CreateActionLogResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        timeFromMidnight =
            toFloat (round model.theTime % (round (24.0 * Time.hour)))

        timeFromStartOfWeek =
            ((daysAwayFromMonday model.theTime) * 24.0 * Time.hour) - timeFromMidnight

        mostRecentMonday =
            model.theTime - timeFromStartOfWeek
    in
        case msg of
            NoOp ->
                model ! [ Cmd.none ]

            CurrentTime time ->
                { model | theTime = time } ! [ getActions, getActionLogs ]

            GetActionsResponse (Ok newActions) ->
                { model | actions = newActions } ! [ Cmd.none ]

            GetActionLogsResponse (Ok newActionLogs) ->
                { model
                    | actionLogs = newActionLogs
                    , weeklyActionLogs =
                        List.filter
                            (\l ->
                                case Date.fromString l.createdAt of
                                    Ok date ->
                                        Date.toTime date >= mostRecentMonday

                                    Err _ ->
                                        False
                            )
                            newActionLogs
                }
                    ! [ Cmd.none ]

            -- TODO: Maybe retry on err?
            GetActionsResponse (Err _) ->
                model ! [ Cmd.none ]

            GetActionLogsResponse (Err _) ->
                model ! [ Cmd.none ]

            CreateActionLog action ->
                model ! [ postActionLog action ]

            CreateActionLogResponse (Ok _) ->
                model ! [ Task.perform CurrentTime Time.now ]

            CreateActionLogResponse (Err _) ->
                model ! [ Cmd.none ]


getActions : Cmd Msg
getActions =
    Http.send GetActionsResponse getActionsRequest


getActionsRequest : Http.Request (List Action)
getActionsRequest =
    Http.get "http://localhost:3333/actions" actionDecoder


actionDecoder : Decode.Decoder (List Action)
actionDecoder =
    Decode.list <|
        Decode.map5 Action
            (Decode.field "id" Decode.int)
            (Decode.field "name" Decode.string)
            (Decode.field "value" Decode.int)
            (Decode.field "per_week" Decode.int)
            (Decode.field "per_day" Decode.int)


getActionLogs : Cmd Msg
getActionLogs =
    Http.send GetActionLogsResponse <| actionLogsRequest


actionLogsRequest : Http.Request (List ActionLog)
actionLogsRequest =
    Http.get "http://localhost:3333/action_logs" actionLogDecoder


postActionLog : Action -> Cmd Msg
postActionLog action =
    Http.send CreateActionLogResponse <| postActionLogRequest action


postActionLogRequest : Action -> Http.Request String
postActionLogRequest action =
    let
        postActionLogUrl =
            "http://localhost:3333/rpc/create_action_log"

        json =
            Encode.object <| [ ( "action_id", Encode.int action.id ) ]

        body =
            Http.jsonBody json
    in
        Http.post postActionLogUrl body (Decode.string)


daysAwayFromMonday : Time -> Float
daysAwayFromMonday time =
    case (Date.dayOfWeek <| Date.fromTime time) of
        Date.Mon ->
            0.0

        Date.Tue ->
            1.0

        Date.Wed ->
            2.0

        Date.Thu ->
            3.0

        Date.Fri ->
            4.0

        Date.Sat ->
            5.0

        Date.Sun ->
            6.0


actionLogDecoder : Decode.Decoder (List ActionLog)
actionLogDecoder =
    Decode.list <|
        Decode.map5 ActionLog
            (Decode.field "id" Decode.int)
            (Decode.field "name" Decode.string)
            (Decode.field "value" Decode.int)
            (Decode.field "action_id" Decode.int)
            (Decode.field "created_at" Decode.string)


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ div [ class "col" ] [ h1 [] [ text "Routinely!" ] ]
            , viewPoints model.actionLogs
            ]
        , div [ class "row" ]
            [ div [ class "col" ] [ viewActionsTable model ] ]
        ]


viewActionsTable : Model -> Html Msg
viewActionsTable model =
    table [ class "table table-bordered" ]
        [ thead []
            [ viewTableHeaders ]
        , tbody []
            (viewWeeklyActions model.actions model.weeklyActionLogs)
        ]


weekDays : List Day
weekDays =
    [ Date.Mon, Date.Tue, Date.Wed, Date.Thu, Date.Fri, Date.Sat, Date.Sun ]


viewTableHeaders : Html Msg
viewTableHeaders =
    tr []
        ([ th [] [ text "" ] ]
            ++ List.map (\d -> th [] [ text <| toString d ]) weekDays
        )


viewWeeklyActions : List Action -> List ActionLog -> List (Html Msg)
viewWeeklyActions actions weeklyLogs =
    List.map
        (\action ->
            let
                logsForAction =
                    (List.filter (\l -> l.actionId == action.id) weeklyLogs)
            in
                tr [ class <| classesForActionRow action logsForAction ] <|
                    [ viewActionLabelCell action ]
                        ++ viewWeekDay action logsForAction
        )
        actions


viewActionLabelCell : Action -> Html Msg
viewActionLabelCell action =
    td [ onClick <| CreateActionLog action ] [ text action.name ]


classesForActionRow : Action -> List ActionLog -> String
classesForActionRow action logsForAction =
    if List.length logsForAction >= action.perWeek then
        "action-week-complete"
    else
        "action-week-incomplete"


viewWeekDay : Action -> List ActionLog -> List (Html Msg)
viewWeekDay action logsForAction =
    List.map
        (\d ->
            td [ class <| classesForActionCell d action logsForAction ]
                (viewLogsForDay d logsForAction)
        )
        weekDays


classesForActionCell : Day -> Action -> List ActionLog -> String
classesForActionCell d action logs =
    if List.length (logsForDay d logs) >= action.perDay then
        "action-day-complete"
    else
        "action-day-incomplete"


viewLogsForDay : Day -> List ActionLog -> List (Html Msg)
viewLogsForDay day logs =
    List.map (\_ -> viewStar) (logsForDay day logs)


viewStar : Html Msg
viewStar =
    span [ class "glyphicon glyphicon-star" ] []


viewPoints : List ActionLog -> Html Msg
viewPoints logs =
    div [ class "col", style [ ( "text-align", "right" ) ] ]
        [ h3 [] [ text <| "Points: " ++ (pointsTotalStr logs) ] ]


pointsTotalStr : List ActionLog -> String
pointsTotalStr logs =
    List.map .value logs |> List.sum |> toString


logsForDay : Day -> List ActionLog -> List ActionLog
logsForDay day =
    List.filter
        (\log ->
            case Date.fromString log.createdAt of
                Ok date ->
                    Date.dayOfWeek date == day

                Err _ ->
                    False
        )
