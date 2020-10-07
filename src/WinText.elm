module WinText exposing (view)

import Random


requiredString : ( Float, String )
requiredString =
    ( 10, "You win!" )


strings : List ( Float, String )
strings =
    [ ( 3, "A winner is you!" )
    , ( 0.1, "All our win are belong to you!" )
    , ( 1, "Completed!" )
    , ( 2, "Congratulations!" )
    , ( 1, "Congratulations! You win!" )
    , ( 5, "Success!" )
    , ( 2, "Puzzle complete!" )
    , ( 2, "Good job!" )
    , ( 2, "Well done!" )
    , ( 1, "Nicely done!" )
    , ( 1, "Slick moves!" )
    ]


text : Random.Generator String
text =
    Random.weighted requiredString strings


view : Int -> String
view =
    Random.initialSeed >> Random.step text >> Tuple.first
