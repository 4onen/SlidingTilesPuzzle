module NumberInput exposing (NumberEntry(..), enteredNumber, entryText, input, stringNumberEntry)

import Element exposing (Element)
import Element.Input


type NumberEntry
    = Empty
    | Minus
    | Num Int


enteredNumber : NumberEntry -> Maybe Int
enteredNumber ne =
    case ne of
        Num n ->
            Just n

        _ ->
            Nothing


entryText : NumberEntry -> String
entryText ne =
    case ne of
        Num n ->
            String.fromInt n

        Minus ->
            "-"

        Empty ->
            ""


stringNumberEntry : NumberEntry -> String -> NumberEntry
stringNumberEntry old str =
    case str of
        "" ->
            Empty

        "-" ->
            Minus

        _ ->
            case String.toInt str of
                Nothing ->
                    old

                Just n ->
                    Num n


input : String -> Maybe String -> (NumberEntry -> msg) -> NumberEntry -> Element msg
input label placeholder tag value =
    Element.Input.text []
        { onChange = stringNumberEntry value >> tag
        , text = entryText value
        , placeholder = Maybe.map (Element.text >> Element.Input.placeholder []) placeholder
        , label = Element.Input.labelAbove [] (Element.text label)
        }
