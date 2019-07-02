module Utility exposing (getListElement)


getListElement : Int -> List a -> Maybe a
getListElement k list =
    list |> List.drop (k - 1) |> List.head
