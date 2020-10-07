module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Html.Events.Extra.Pointer
import Json.Decode
import NumberInput exposing (NumberEntry(..))
import Random
import Sliding exposing (Board, Move(..))
import Task


type Msg
    = Sidebar SidebarMsg
    | ClassifyDevice Int Int
    | TouchStart ( Float, Float )
    | TouchEnd ( Float, Float )
      -- Generation:
    | GeneratedBoard ( Board, Int )
    | GenerateSeededBoard Sliding.Size Int
    | GenerateRandomBoard Sliding.Size
      -- Gameplay:
    | MakeMove Move


type SidebarMsg
    = ToggleSidebar
    | SetOnscreenControls Bool
    | ChangeSeed NumberEntry
    | ChangeWidth NumberEntry
    | ChangeHeight NumberEntry


type alias Model =
    { device : Element.Device
    , sidebar : SidebarModel
    , touch : Maybe ( Float, Float )
    , game : Maybe ( Board, Int )
    }


type alias SidebarModel =
    { nextSize : ( NumberEntry, NumberEntry )
    , nextSeed : NumberEntry
    , onscreenControlsVisible : Bool
    , sidebarVisible : Bool
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        defaultSize =
            ( 3, 3 )

        justDefaultSize =
            Tuple.mapBoth Num Num defaultSize

        defaultDevice =
            Element.Device Element.Desktop Element.Landscape

        sidebarinit =
            SidebarModel justDefaultSize Empty True True
    in
    ( Model defaultDevice sidebarinit Nothing Nothing
    , Cmd.batch
        [ Random.generate GeneratedBoard (Sliding.seededBoard defaultSize)
        , Task.perform (.viewport >> (\{ width, height } -> ClassifyDevice (floor width) (floor height))) Browser.Dom.getViewport
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Browser.Events.onResize ClassifyDevice, keySubs model ]


keySubs : Model -> Sub Msg
keySubs { game } =
    let
        controls =
            [ ( [ "ArrowLeft", "a", "A", "h", "H" ], Sliding.East )
            , ( [ "ArrowRight", "d", "D", "l", "L" ], Sliding.West )
            , ( [ "ArrowUp", "w", "W", "k", "K" ], Sliding.South )
            , ( [ "ArrowDown", "s", "S", "j", "J" ], Sliding.North )
            ]
                |> List.concatMap (\( ctrlList, action ) -> List.map (\ctrl -> ( ctrl, action )) ctrlList)
                |> Dict.fromList

        stringToMove : String -> Maybe Move
        stringToMove key =
            Dict.get key controls

        decodeMove : Json.Decode.Decoder Msg
        decodeMove =
            Json.Decode.andThen (Maybe.map (MakeMove >> Json.Decode.succeed) >> Maybe.withDefault (Json.Decode.fail "No valid move conversion")) <| Json.Decode.map stringToMove (Json.Decode.field "key" Json.Decode.string)
    in
    case Maybe.map (Tuple.first >> Sliding.won) game of
        Just False ->
            Browser.Events.onKeyDown decodeMove

        _ ->
            Sub.none


nocmd : model -> ( model, Cmd msg )
nocmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Sidebar sidebarMsg ->
            nocmd { model | sidebar = updateSidebar sidebarMsg model.sidebar }

        ClassifyDevice width height ->
            nocmd { model | device = Element.classifyDevice { width = width, height = height } }

        TouchStart coords ->
            nocmd { model | touch = Just coords }

        TouchEnd ( endx, endy ) ->
            case model.touch of
                Nothing ->
                    nocmd model

                Just ( startx, starty ) ->
                    case translateMov (endx - startx) (endy - starty) of
                        Nothing ->
                            nocmd model

                        Just move ->
                            update (MakeMove move) model

        GeneratedBoard seededBoard ->
            nocmd { model | game = Just seededBoard }

        GenerateSeededBoard size seed ->
            nocmd { model | game = Just ( Sliding.boardFromSeed size seed, seed ) }

        GenerateRandomBoard size ->
            ( model, Random.generate GeneratedBoard <| Sliding.seededBoard size )

        MakeMove move ->
            nocmd
                { model
                    | game =
                        Maybe.map
                            (Tuple.mapFirst
                                (\board ->
                                    Maybe.withDefault board <| Sliding.move move board
                                )
                            )
                            model.game
                }


updateSidebar : SidebarMsg -> SidebarModel -> SidebarModel
updateSidebar msg model =
    case msg of
        ChangeSeed v ->
            { model | nextSeed = v }

        ChangeWidth v ->
            { model | nextSize = ( Tuple.first model.nextSize, v ) }

        ChangeHeight v ->
            { model | nextSize = ( v, Tuple.second model.nextSize ) }

        ToggleSidebar ->
            { model | sidebarVisible = not model.sidebarVisible }

        SetOnscreenControls v ->
            { model | onscreenControlsVisible = v }


translateMov : Float -> Float -> Maybe Move
translateMov movx movy =
    if movx * movx + movy * movy > 200 then
        Just <|
            if abs movx > abs movy then
                -- horizontal
                if movx > 0 then
                    West

                else
                    East

            else
            -- vertical
            if
                movy > 0
            then
                North

            else
                South

    else
        Nothing


myOnPointerDown : Element.Attribute Msg
myOnPointerDown =
    Element.htmlAttribute <|
        Html.Events.Extra.Pointer.onWithOptions
            "pointerdown"
            { stopPropagation = True, preventDefault = False }
            (.pointer >> .pagePos >> TouchStart)


myOnPointerUp : Element.Attribute Msg
myOnPointerUp =
    Element.htmlAttribute <|
        Html.Events.Extra.Pointer.onWithOptions
            "pointerup"
            { stopPropagation = True, preventDefault = False }
            (.pointer >> .pagePos >> TouchEnd)


myOnTouchDown : Element.Attribute Msg
myOnTouchDown =
    Element.htmlAttribute <|
        Html.Events.Extra.Pointer.onWithOptions
            "touchstart"
            { stopPropagation = True, preventDefault = False }
            (.pointer >> .pagePos >> TouchStart)


myOnTouchUp : Element.Attribute Msg
myOnTouchUp =
    Element.htmlAttribute <|
        Html.Events.Extra.Pointer.onWithOptions
            "touchend"
            { stopPropagation = True, preventDefault = False }
            (.pointer >> .pagePos >> TouchEnd)


responsiveEnlargenCondition : Element.Device -> Bool
responsiveEnlargenCondition { class, orientation } =
    class == Element.BigDesktop || orientation == Element.Portrait


responsiveText : Int -> Element.Device -> Element.Attribute msg
responsiveText base device =
    Element.Font.size <|
        if responsiveEnlargenCondition device then
            base * 2

        else
            base


view : Model -> Html.Html Msg
view model =
    Element.layout [ myOnPointerDown, myOnTouchDown, myOnPointerUp, myOnTouchUp, responsiveText 16 model.device ] <|
        Element.el
            [ Element.width Element.fill
            , Element.onLeft <| viewSidebar model.device model.sidebar
            ]
            (case model.game of
                Nothing ->
                    Element.paragraph []
                        [ Element.text "Generate a board to start! If you've already clicked one of the generator buttons and it's taking some time, you may need to refresh the page and try a smaller board." ]

                Just ( game, seed ) ->
                    Element.column [ Element.spacing 5, Element.width Element.fill ]
                        [ (String.fromInt >> (++) "Seed: " >> Element.text >> Element.el [ Element.centerX, Element.alignTop ]) seed
                        , Sliding.view [ Element.centerX, Element.centerY ] game
                        , if Sliding.won game then
                            Element.el [ Element.centerX, Element.scale 2.0 ] <| Element.text "You win!"

                          else if model.sidebar.onscreenControlsVisible then
                            viewControls [ Element.centerX, Element.spacing 20, responsiveText 30 model.device ] game

                          else
                            Element.none
                        ]
            )


viewControls : List (Element.Attribute Msg) -> Board -> Element Msg
viewControls attrs game =
    let
        controls =
            [ ( "←", Sliding.East )
            , ( "↓", Sliding.North )
            , ( "↑", Sliding.South )
            , ( "→", Sliding.West )
            ]
    in
    controls
        |> List.map
            (\( label, move ) ->
                Element.Input.button [ Element.width <| Element.px 64, Element.height <| Element.px 64, Element.Font.center, Element.Border.width 1 ]
                    { label = Element.text label
                    , onPress = game |> Sliding.move move |> Maybe.map (always (MakeMove move))
                    }
            )
        |> Element.row attrs


viewSidebar : Element.Device -> SidebarModel -> Element Msg
viewSidebar device { nextSeed, nextSize, onscreenControlsVisible, sidebarVisible } =
    let
        controlsWidth =
            if responsiveEnlargenCondition device then
                353

            else
                222

        visibilityToggle =
            Element.Input.button [ Element.moveRight controlsWidth, responsiveText 30 device ]
                { onPress = Just <| Sidebar ToggleSidebar
                , label =
                    Element.text <|
                        if sidebarVisible then
                            "<<"

                        else
                            ">>"
                }
    in
    if not sidebarVisible then
        visibilityToggle

    else
        Element.column
            [ Element.alignLeft
            , Element.Border.width 1
            , Element.spacing 10
            , Element.width <| Element.px controlsWidth
            , Element.moveRight controlsWidth
            , Element.onLeft <| visibilityToggle
            , Element.Background.color (Element.rgb 1 1 1)
            ]
            (viewControlsControls onscreenControlsVisible
                :: viewBoardGeneratorControls nextSize nextSeed
            )


viewBoardGeneratorControls : ( NumberEntry, NumberEntry ) -> NumberEntry -> List (Element Msg)
viewBoardGeneratorControls (( nextHeight, nextWidth ) as nextSize) nextSeed =
    let
        sizeError =
            [ Element.paragraph [] [ Element.text "To guarantee a solvable board, width and height must both be greater than one." ] ]

        heightWidthEntry =
            Element.row []
                [ NumberInput.input "Height" (Just "Positive number") (Sidebar << ChangeHeight) nextHeight
                , NumberInput.input "Width" (Just "Positive number") (Sidebar << ChangeWidth) nextWidth
                ]
    in
    List.append
        [ Element.text "Next game:"
        , heightWidthEntry
        ]
        (case nextSize of
            ( Num height, Num width ) ->
                if width > 0 && height > 0 then
                    [ Element.Input.button [ Element.centerX, Element.Border.width 1 ]
                        { onPress = Just (GenerateRandomBoard ( height, width ))
                        , label = Element.text "Generate random board!"
                        }
                    , NumberInput.input "Seed" (Just "Number") (Sidebar << ChangeSeed) nextSeed
                    , nextSeed
                        |> NumberInput.enteredNumber
                        |> Maybe.map
                            (\seed ->
                                Element.Input.button [ Element.centerX, Element.Border.width 1 ]
                                    { onPress = Just (GenerateSeededBoard ( height, width ) seed)
                                    , label = Element.text "Generate from seed!"
                                    }
                            )
                        |> Maybe.withDefault (Element.text "Seed must be a number!")
                    ]

                else
                    sizeError

            _ ->
                sizeError
        )


viewControlsControls : Bool -> Element Msg
viewControlsControls onscreenControlsVisible =
    Element.column []
        [ Element.text "Controls:"
        , Element.paragraph [ Element.Font.size 14, Element.paddingXY 20 0 ]
            [ Element.text "WASD,"
            , Element.text "HJKL,"
            , Element.text "↑←↓→,"
            , Element.text "and tap/click dragging"
            ]
        , Element.Input.checkbox []
            { checked = onscreenControlsVisible
            , icon = Element.Input.defaultCheckbox
            , label = Element.Input.labelRight [] <| Element.text "On-screen controls"
            , onChange = SetOnscreenControls >> Sidebar
            }
        ]
