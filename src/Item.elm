module Item exposing (Item, checkItem, encodeItem, isDone, showItem, mkItem)

import Html exposing (Html, button, label, li, input, span, text)
import Html.Attributes exposing (type_, class, checked, classList)
import Html.Events exposing (onClick)
import Json.Encode as E

type Item = Item { done : Bool, value : String }


isDone : Item -> Bool
isDone (Item item) = item.done


checkItem : Int -> Int -> Item -> Item
checkItem checkedId itemId (Item item) =
    if checkedId == itemId then
        Item { item | done = not item.done }
    else
        Item item


encodeItem : Item -> E.Value
encodeItem (Item item) =
    E.object [ ("done", E.bool item.done)
             , ("value", E.string item.value)
             ]


mkItem : String -> Maybe Item
mkItem value =
    if String.isEmpty value then
        Nothing
    else
        Just <| Item { done = False, value = value }


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


deleteButton : msg -> Html msg
deleteButton deleteMsg =
    button [ class "delete"
           , onClick deleteMsg
           ]
        [ text "X" ]


showItem : (Int -> msg) -> (Int -> msg) -> Int -> Item -> Html msg
showItem checkMsg deleteMsg id (Item item) =
    li []
        [ checkbox (checkMsg id) item.done item.value
        , deleteButton (deleteMsg id)
        ]
