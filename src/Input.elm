module Input exposing (load)

import Http


load : Int -> (Int -> Result Http.Error String -> msg) -> Cmd msg
load day toMsg =
    Http.get
        { url = "/input/" ++ String.fromInt day ++ ".txt"
        , expect = Http.expectString (toMsg day)
        }
