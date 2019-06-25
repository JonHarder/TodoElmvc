import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Html exposing (Html, Attribute, button, div, form, h1, input, label, li, span, text, ol)
import Html.Attributes exposing (class, classList, checked, placeholder, style, type_)
import Html.Events exposing (on, onSubmit, onInput, onClick, keyCode, targetValue)
import Json.Decode as Json
import Url exposing (Url)


type Item = Item { done : Bool, value : String }

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
    ( { key = key, items = [], newTodo = "" }, Cmd.none )


-------------------------------------------------------------------------------
-- VIEW
view : Model -> Html Msg
view model = 
    let
        (done, notDone) =
            List.partition (\(Item item) -> item.done) model.items
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


checkbox : msg -> Bool -> String -> Html msg
checkbox msg isChecked name =
    label
        []
        [ input
              [ type_ "checkbox"
              , checked isChecked
              , onClick msg
              ] []
        , span [ classList [("strikethrough", isChecked)] ] [ text name ]
        ]


deleteButton : Int -> Html Msg
deleteButton id =
    button [ class "delete"
           , onClick (DeleteItem id)
           ]
        [ text "X" ]


showItem : Int -> Item -> Html Msg
showItem id (Item item) =
    li []
        [ checkbox (Check id) item.done item.value
        , deleteButton id
        ]
    

showTodo : List Item -> Html Msg
showTodo items =
    ol []
      (List.append (List.indexedMap showItem items) [newTodo])


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


checkItem : Int -> Int -> Item -> Item
checkItem checkedId itemId (Item item) =
    if checkedId == itemId then
        Item { item | done = not item.done }
    else
        Item item


updateItems : Int -> List Item -> List Item
updateItems checkedId items =
    List.indexedMap (checkItem checkedId) items


addTodo : String -> List Item -> List Item
addTodo value items =
    if String.isEmpty value then
        items
    else
        List.append items [ Item { done = False, value = value } ] 


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
            ( { model | items = updateItems id model.items }
            , Cmd.none
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
                todoValue = model.newTodo
            in
                ( { model | items = addTodo todoValue model.items
                          , newTodo = ""
                  }
                , Cmd.none
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
