port module Ports.LocalStoragePort exposing (clear, getItem, listKeys, response, setItem)

import LocalStorage exposing (ClearPort, GetItemPort, ListKeysPort, ResponsePort, SetItemPort)


port getItem : GetItemPort msg


port setItem : SetItemPort msg


port clear : ClearPort msg


port listKeys : ListKeysPort msg


port response : ResponsePort msg
