import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Html exposing (Html, Attribute, button, div, h1, input, label, li, text, ol)
import Html.Attributes exposing (checked, placeholder, style, type_)
import Html.Events exposing (on, onClick, keyCode, targetValue)
import Json.Decode as Json
import Url exposing (Url)


type Item = Item { done : Bool, value : String }

type alias Model =
    { key : Key
    , items: List Item
    }

type Msg
    = NoOp
    | Check Int
    | NewTodo String


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let items =
            [ Item {done = False
                   , value = "do the thing"
                   }
            , Item {done = False
                   , value = "and then do the other thing"
                   }
            ]
    in ( { key = key, items = items }, Cmd.none )


onEnter : (String -> Msg ) -> Attribute Msg
onEnter toMsg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed "enter pressed"
            else
                Json.fail "not Enter"
        decodeEnter =
            Json.andThen isEnter keyCode
    in
        on "keydown" (Json.map2 (\key value -> toMsg value) decodeEnter targetValue)

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
        , text name
        ]


showItem : Int -> Item -> Html Msg
showItem id (Item item) =
    li []
        [ checkbox (Check id) item.done item.value ]
    

showTodo : List Item -> Html Msg
showTodo items =
    if (List.length items) > 0 then
        ol []
          (List.append (List.indexedMap showItem items) [newTodo])
    else
        text "Nothing to do"


newTodo : Html Msg
newTodo =
    li []
        [ input
              [ placeholder "Add another"
              , onEnter NewTodo
              ]
              []
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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Check id ->
            ( { model | items = updateItems id model.items }
            , Cmd.none
            )

        NewTodo value ->
            ( { model | items = addTodo value model.items }
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
