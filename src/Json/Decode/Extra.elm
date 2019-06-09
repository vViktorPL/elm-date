module Json.Decode.Extra exposing (fromMaybe)

import Json.Decode exposing (Decoder)


fromMaybe : String -> Maybe a -> Decoder a
fromMaybe error maybe =
    case maybe of
        Just value ->
            Json.Decode.succeed value

        Nothing ->
            Json.Decode.fail error
