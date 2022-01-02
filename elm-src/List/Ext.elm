module List.Ext exposing (..)


find : (a -> Bool) -> List a -> Maybe a
find p xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            if p x then
                Just x

            else
                find p rest
