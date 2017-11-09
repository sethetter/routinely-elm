module Routinely exposing (..)

import Dialog
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Time exposing (Time)
import Time.Date exposing (Weekday)
import Time.DateTime as DT
import Time.TimeZones exposing (us_central)
import Time.ZonedDateTime as ZDT exposing (ZonedDateTime)
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
    , selectedAction : Maybe Action
    , actionLogs : List ActionLog
    , weeklyActionLogs : List ActionLog
    , theTime : Time
    , showDialog : Bool
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
      , selectedAction = Nothing
      , actionLogs = []
      , weeklyActionLogs = []
      , theTime = 0.0
      , showDialog = False
      }
    , Task.perform CurrentTime Time.now
    )


type Msg
    = NoOp
    | AcknowledgeCreateActionLog Action
    | CloseDialog
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
            ((daysAwayFromMonday model.theTime) * 24.0 * Time.hour) + timeFromMidnight

        mostRecentMonday =
            model.theTime - timeFromStartOfWeek
    in
        case msg of
            NoOp ->
                model ! [ Cmd.none ]

            CloseDialog ->
                { model
                    | showDialog = False
                    , selectedAction = Nothing
                }
                    ! [ Cmd.none ]

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
                                case strToZonedDateTime l.createdAt of
                                    Ok date ->
                                        ZDT.toTimestamp date >= mostRecentMonday

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
                { model
                    | showDialog = True
                    , selectedAction = Just action
                }
                    ! [ Cmd.none ]

            AcknowledgeCreateActionLog action ->
                { model | showDialog = False } ! [ postActionLog action ]

            CreateActionLogResponse (Ok _) ->
                model ! [ Task.perform CurrentTime Time.now ]

            CreateActionLogResponse (Err _) ->
                model ! [ Cmd.none ]


strToZonedDateTime : String -> Result String ZonedDateTime
strToZonedDateTime str =
    ZDT.fromISO8601 (us_central ()) <| str ++ "+00:00"


apiPrefix : String
apiPrefix =
    "/api"


apiRoute : String -> String
apiRoute path =
    apiPrefix ++ path


getActions : Cmd Msg
getActions =
    Http.send GetActionsResponse getActionsRequest


getActionsRequest : Http.Request (List Action)
getActionsRequest =
    Http.get (apiRoute "/actions") actionDecoder


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
    Http.get (apiRoute "/action_logs") actionLogDecoder


postActionLog : Action -> Cmd Msg
postActionLog action =
    Http.send CreateActionLogResponse <| postActionLogRequest action


postActionLogRequest : Action -> Http.Request String
postActionLogRequest action =
    let
        postActionLogUrl =
            apiRoute "/rpc/create_action_log"

        json =
            Encode.object <| [ ( "action_id", Encode.int action.id ) ]

        body =
            Http.jsonBody json
    in
        Http.post postActionLogUrl body Decode.string


daysAwayFromMonday : Time -> Float
daysAwayFromMonday time =
    case (DT.weekday <| DT.fromTimestamp time) of
        Time.Date.Mon ->
            0.0

        Time.Date.Tue ->
            1.0

        Time.Date.Wed ->
            2.0

        Time.Date.Thu ->
            3.0

        Time.Date.Fri ->
            4.0

        Time.Date.Sat ->
            5.0

        Time.Date.Sun ->
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
        [ div [ class "row top" ]
            [ div [ class "col" ] [ h1 [] [ span [ class "align-middle" ] [ text "Routinely!" ] ] ]
            , viewPoints model.actionLogs
            ]
        , div [ class "row" ]
            [ div [ class "col" ] [ viewActionsTable model ] ]
        , div [ class "row" ]
            [ div [ class "col" ] [ viewRewardMeter model ] ]
        , div [ class "row" ]
            [ div [ class "col" ] [ viewRewardsToRedeem model.actionLogs ] ]
        , Dialog.view
            (if model.showDialog then
                case model.selectedAction of
                    Just action ->
                        Just (actionConfirmDialogConfig action)

                    Nothing ->
                        Nothing
             else
                Nothing
            )
        ]


actionConfirmDialogConfig : Action -> Dialog.Config Msg
actionConfirmDialogConfig action =
    { body = Just (text "Are you sure?")
    , closeMessage = Just CloseDialog
    , containerClass = Nothing
    , footer =
        Just
            (button
                [ class "btn btn-success"
                , onClick <| AcknowledgeCreateActionLog action
                ]
                [ text "OK" ]
            )
    , header = Just (h3 [] [ text action.name ])
    }


viewActionsTable : Model -> Html Msg
viewActionsTable model =
    table [ class "table table-bordered" ]
        [ thead []
            [ viewTableHeaders ]
        , tbody []
            (viewWeeklyActions model.actions model.weeklyActionLogs)
        ]


weekDays : List Weekday
weekDays =
    [ Time.Date.Mon, Time.Date.Tue, Time.Date.Wed, Time.Date.Thu, Time.Date.Fri, Time.Date.Sat, Time.Date.Sun ]


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


classesForActionCell : Weekday -> Action -> List ActionLog -> String
classesForActionCell d action logs =
    if List.length (logsForDay d logs) >= action.perDay then
        "action-day-complete"
    else
        "action-day-incomplete"


viewLogsForDay : Weekday -> List ActionLog -> List (Html Msg)
viewLogsForDay day logs =
    List.map (\_ -> (viewIcon "star")) (logsForDay day logs)


viewIcon : String -> Html Msg
viewIcon icon =
    span [ class <| "oi oi-" ++ icon ] []


viewPoints : List ActionLog -> Html Msg
viewPoints logs =
    div [ class "col", style [ ( "text-align", "right" ) ] ]
        [ h2 [ class "align-middle" ] [
               span [ class "badge badge-pill badge-warning" ] [ text <| (pointsTotalStr logs) ] ] ]


pointsTotalStr : List ActionLog -> String
pointsTotalStr logs =
    List.map .value logs |> List.sum |> toString


logsForDay : Weekday -> List ActionLog -> List ActionLog
logsForDay day =
    List.filter
        (\log ->
            -- Convert createdAt to time, add TZ offset, and then convert to date
            case strToZonedDateTime log.createdAt of
                Ok date ->
                    ZDT.weekday date == day

                Err _ ->
                    False
        )


viewRewardMeter : Model -> Html Msg
viewRewardMeter model =
    let
        baseClasses =
            "progress-bar progress-bar-striped progress-bar-animated "

        percentToNextReward =
            progressToReward model.actionLogs
    in
        div [ class "progress" ]
            [ div
                [ class (baseClasses ++ (colorClassForPercent percentToNextReward))
                , style [ ( "width", (toString percentToNextReward) ++ "%" ) ]
                ]
                []
            ]


colorClassForPercent : Int -> String
colorClassForPercent p =
    if p <= 33 then
        "bg-danger"
    else if p < 66 then
        "bg-warning"
    else if p < 90 then
        "bg-info"
    else
        "bg-success"


progressToReward : List ActionLog -> Int
progressToReward logs =
    let
        totalPoints =
            List.sum <| List.map .value logs
    in
        (rem totalPoints 50) * 2


viewRewardsToRedeem : List ActionLog -> Html Msg
viewRewardsToRedeem logs =
    let
        gift =
            span [ style [ ( "color", "gold" ), ( "font-size", "2em" ) ] ]
                [ viewIcon "gift", text " " ]

        numberOfRewards =
            (List.sum <| List.map .value logs) // 50
    in
        h1 [ class "text-right" ]
            (List.repeat numberOfRewards gift)
