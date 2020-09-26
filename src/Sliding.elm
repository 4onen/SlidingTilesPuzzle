module Sliding exposing (Board, Move(..), Size, board, boardFromSeed, move, seededBoard, solvable, view, won)

import Bitwise
import Element exposing (Element)
import Element.Border
import Element.Font
import List.Extra
import Random exposing (Generator)


type alias Size =
    ( Int, Int )


type alias Board =
    { width : Int
    , board : List Int
    , space : Int
    }


type Move
    = North
    | South
    | East
    | West


moveSpaceTo : Int -> Board -> Maybe Board
moveSpaceTo target game =
    let
        targetIdx =
            if target > game.space then
                target - 1

            else
                target
    in
    game.board
        |> List.Extra.getAt targetIdx
        |> Maybe.map
            (\targetValue ->
                let
                    boardWithoutTarget =
                        List.Extra.removeAt targetIdx game.board

                    spaceWithoutTargetIndex =
                        if target < game.space then
                            game.space - 1

                        else
                            game.space

                    ( beginning, end ) =
                        List.Extra.splitAt spaceWithoutTargetIndex boardWithoutTarget
                in
                { game | board = List.append beginning (targetValue :: end), space = target }
            )


won : Board -> Bool
won game =
    (List.sort game.board == game.board) && (game.space == List.length game.board)


move : Move -> Board -> Maybe Board
move dir game =
    case dir of
        North ->
            moveSpaceTo (game.space - game.width) game

        South ->
            moveSpaceTo (game.space + game.width) game

        East ->
            if Basics.modBy game.width game.space < game.width - 1 then
                Just { game | space = game.space + 1 }

            else
                Nothing

        West ->
            if Basics.modBy game.width game.space > 0 then
                Just { game | space = game.space - 1 }

            else
                Nothing


view : List (Element.Attribute msg) -> Board -> Element msg
view attrs game =
    let
        tileel is_space =
            Element.el <|
                (if not is_space then
                    (::) (Element.Border.width 1)

                 else
                    Basics.identity
                )
                <|
                    [ Element.width <| Element.px 36, Element.height <| Element.px 36, Element.Font.center ]

        renderedBoard =
            game.board
                |> List.map (String.fromInt >> Element.text >> tileel False)
                |> List.Extra.splitAt game.space
                |> (\( beginning, end ) -> List.append beginning (tileel True Element.none :: end))
                |> List.Extra.greedyGroupsOf game.width
                |> List.map (Element.row [ Element.width Element.fill, Element.centerX ])
                |> Element.column attrs
    in
    renderedBoard


listPermutation : List a -> Generator (List a)
listPermutation l =
    case l of
        [] ->
            Random.constant []

        first :: [] ->
            Random.constant [ first ]

        first :: rest ->
            Random.uniform first rest
                |> Random.andThen
                    (\selectedValue ->
                        let
                            withoutValue =
                                List.Extra.remove selectedValue l
                        in
                        Random.map ((::) selectedValue) (listPermutation withoutValue)
                    )


makeSolvable : Board -> Board
makeSolvable game =
    let
        isEven : Int -> Bool
        isEven =
            Bitwise.and 1 >> (==) 0

        inversionsParityOdd : Bool
        inversionsParityOdd =
            List.Extra.indexedFoldl
                (\idx val parity ->
                    game.board
                        |> List.drop (idx + 1)
                        |> List.map ((>) val)
                        |> List.foldl Basics.xor False
                        |> Basics.xor parity
                )
                False
                game.board

        swapFirstTwo : List a -> List a
        swapFirstTwo l =
            case l of
                a :: b :: rest ->
                    b :: a :: rest

                _ ->
                    l

        height : Int
        height =
            1 + (List.length game.board // game.width)

        spaceRow : Int
        spaceRow =
            game.space // game.width

        spaceRowFromBottomOneIndexed : Int
        spaceRowFromBottomOneIndexed =
            height - spaceRow
    in
    case ( isEven game.width, isEven spaceRowFromBottomOneIndexed, inversionsParityOdd ) of
        ( False, _, True ) ->
            { game | board = swapFirstTwo game.board }

        ( False, _, False ) ->
            game

        ( True, True, True ) ->
            game

        ( True, True, False ) ->
            { game | board = swapFirstTwo game.board }

        ( True, False, True ) ->
            { game | board = swapFirstTwo game.board }

        ( True, False, False ) ->
            game


solvable : Board -> Bool
solvable game =
    makeSolvable game == game


board : Size -> Generator Board
board ( height, width ) =
    let
        lastBoardNumber : Int
        lastBoardNumber =
            width * height - 1

        permutationGenerator : Generator (List Int)
        permutationGenerator =
            listPermutation <| List.range 1 lastBoardNumber

        spaceLocationGenerator =
            Random.int 0 <| lastBoardNumber - 1
    in
    Random.map makeSolvable <|
        Random.map2 (Board width)
            permutationGenerator
            spaceLocationGenerator


boardFromSeed : Size -> Int -> Board
boardFromSeed size =
    Random.initialSeed >> Random.step (board size) >> Tuple.first


seededBoard : Size -> Generator ( Board, Int )
seededBoard size =
    Random.map (\seed -> ( boardFromSeed size seed, seed )) <|
        Random.int Random.minInt Random.maxInt
