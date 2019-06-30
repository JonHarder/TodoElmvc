port module Cache exposing (saveList)

import Json.Encode as E

import Item exposing (..)

port cache : E.Value -> Cmd msg


saveList : List Item -> Cmd msg
saveList items =
    cache <| E.list encodeItem items

