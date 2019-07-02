module Utility exposing (getListElement, uniquefyList)


getListElement : Int -> List a -> Maybe a
getListElement k list =
    list |> List.drop (k - 1) |> List.head


uniquefyList : List comparable -> List comparable
uniquefyList list =
    list
        |> List.sort
        |> List.foldl
            (\item acc ->
                if Just item /= List.head acc then
                    item :: acc

                else
                    acc
            )
            []
