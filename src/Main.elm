import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Html exposing (Html, Attribute, button, div, form, h1, input, label, li, span, text, ol)
import Html.Attributes exposing (class, classList, checked, placeholder, style, type_)
import Html.Events exposing (on, onSubmit, onInput, onClick, keyCode, targetValue)
import Json.Decode as Json
import Json.Encode as E
import Url exposing (Url)

import Item exposing (..)
import Cache exposing (saveList)


type alias Model =
    { key : Key
    , items: List Item
    , newTodo : String
    }

type Msg
    = NoOp
    | Check Int
    | DeleteItem Int
    | UpdateNewTodo String
    | MakeItem


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let items = []
        cmd = Cmd.none -- loadItems items
    in ( { key = key, items = [], newTodo = "" }, cmd )


-------------------------------------------------------------------------------
-- VIEW
view : Model -> Html Msg
view model = 
    let
        (done, notDone) =
            List.partition isDone model.items
        doneCount =
            List.length done
        notDoneCount =
            List.length notDone
    in
        div []
            [ h1 [] [ text "Todo ELM style" ]
            , showTodo model.items
            , div []
                [ text ("Todo: " ++ (String.fromInt notDoneCount))
                , text (" Done: " ++ (String.fromInt doneCount))
                ]
            ]


showTodo : List Item -> Html Msg
showTodo items =
    ol []
      (List.append (List.indexedMap (showItem Check DeleteItem) items) [newTodo])


newTodo : Html Msg
newTodo =
    li []
        [ form [ onSubmit MakeItem ]
              [ input
                    [ placeholder "Add an item"
                    , onInput UpdateNewTodo
                    ]
                    []
              ]
        ]
-----------------------------------------------------------------------------------
-- UPDATE


updateItems : Int -> List Item -> List Item
updateItems checkedId items =
    List.indexedMap (checkItem checkedId) items


addTodo : Item -> List Item -> List Item
addTodo item items =
    List.append items [ item ] 


deleteIndex : Int -> List a -> List a
deleteIndex index items =
    let
        indexed = List.indexedMap Tuple.pair items
        filtered = List.filter (\(i, item) -> i /= index) indexed
    in
        List.map Tuple.second filtered


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Check id ->
            let
                items =
                    updateItems id model.items
            in
                ( { model | items = items }
                , saveList items
                )

        DeleteItem id ->
            ( { model | items = deleteIndex id model.items }
            , Cmd.none
            )

        UpdateNewTodo value ->
            ( { model | newTodo = value }
            , Cmd.none
            )

        MakeItem ->
            let
                item = mkItem model.newTodo
                (items, cmd) =
                    case item of
                        Just i ->
                            let added = addTodo i model.items
                            in (added, saveList added)
                        Nothing ->
                            (model.items, Cmd.none)
            in
                ( { model | items = items , newTodo = "" }
                , cmd
                )


-----------------------------------------------------------------------------------
-- MAIN

docView : Model -> Browser.Document Msg
docView model =
    { title = "Elm Todo"
    , body = [ view model ]
    }

subscriptions model = Sub.none

onUrlRequest urlReq = NoOp

onUrlChange url = NoOp


main = application
       { init = init
       , view = docView
       , update = update
       , subscriptions = subscriptions
       , onUrlRequest = onUrlRequest
       , onUrlChange = onUrlChange
       }
