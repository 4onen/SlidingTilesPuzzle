module Main exposing (main)

import Browser
import Html
import Browser.Events
import Element exposing (Element)
import Element.Border
import Element.Font
import Element.Input
import Json.Decode
import Random
import Sliding exposing (Board, Move(..))
import Html.Events.Extra.Pointer
import Dict
import Task
import Browser.Dom


type Msg
    = Sidebar SidebarMsg
    | ClassifyDevice Int Int
    | TouchStart (Float,Float)
    | TouchEnd (Float,Float)
      -- Generation:
    | GeneratedBoard ( Board, Int )
    | GenerateSeededBoard
    | GenerateRandomBoard
      -- Gameplay:
    | MakeMove Move


type SidebarMsg
    = ToggleVisible
    | ChangeSeed Int
    | ChangeWidth Int
    | ChangeHeight Int


type alias Model =
    { deviceClass : Element.DeviceClass
    , sidebar : SidebarModel
    , touch : Maybe (Float, Float)
    , game : Maybe ( Board, Int )
    }


type alias SidebarModel =
    { nextSeed : Int
    , nextSize : Sliding.Size
    , visible : Bool
    }


main : Program () Model Msg
main =
    Browser.document
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

        sidebarinit =
            SidebarModel 0 defaultSize True
    in
    ( Model Element.Desktop sidebarinit Nothing Nothing
    , Cmd.batch 
        [ Random.generate GeneratedBoard (Sliding.seededBoard defaultSize)
        , Task.perform (.viewport >> (\{width,height} -> ClassifyDevice (floor width) (floor height))) Browser.Dom.getViewport
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [Browser.Events.onResize ClassifyDevice, keySubs model]
    
            

keySubs : Model -> Sub Msg
keySubs {game} =
    let
        controls = 
            [ (["ArrowLeft","a","A","h","H"],Sliding.East)
            , (["ArrowRight","d","D","l","L"],Sliding.West)
            , (["ArrowUp","w","W","k","K"],Sliding.South)
            , (["ArrowDown","s","S","j","J"],Sliding.North)
            ] 
            |> List.concatMap (\(ctrlList,action) -> List.map (\ctrl -> (ctrl,action)) ctrlList) 
            |> Dict.fromList

        stringToMove : String -> Maybe Move
        stringToMove key = Dict.get key controls

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
            nocmd { model | deviceClass = .class <| Element.classifyDevice {width = width,height = height}}
        
        TouchStart coords ->
            nocmd { model | touch = Just coords}
        
        TouchEnd (endx,endy) ->
            case model.touch of
                Nothing ->
                    nocmd model
                Just (startx,starty) ->
                    case translateMov (endx-startx) (endy-starty) of
                        Nothing ->
                            nocmd model
                        Just move ->
                            update (MakeMove move) model


        GeneratedBoard seededBoard ->
            nocmd { model | game = Just seededBoard }

        GenerateSeededBoard ->
            nocmd { model | game = Just ( Sliding.boardFromSeed model.sidebar.nextSize model.sidebar.nextSeed, model.sidebar.nextSeed ) }

        GenerateRandomBoard ->
            ( model, Random.generate GeneratedBoard <| Sliding.seededBoard model.sidebar.nextSize )

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

        ToggleVisible ->
            { model | visible = not model.visible }

translateMov : Float -> Float -> Maybe Move
translateMov movx movy =
    if movx*movx + movy*movy > 200 then
        Just <| if abs movx > abs movy then -- horizontal
            if movx > 0 then West else East
        else -- vertical
            if movy > 0 then North else South
    else
        Nothing

myOnPointerDown : Element.Attribute Msg
myOnPointerDown =
    Element.htmlAttribute <| 
        Html.Events.Extra.Pointer.onWithOptions 
            "pointerdown" 
            { stopPropagation = True, preventDefault = True }
            (.pointer >> .pagePos >> TouchStart)

myOnPointerUp : Element.Attribute Msg
myOnPointerUp =
    Element.htmlAttribute <|
        Html.Events.Extra.Pointer.onWithOptions 
            "pointerup" 
            { stopPropagation = True, preventDefault = True } 
            (.pointer >> .pagePos >> TouchEnd)

myOnTouchDown : Element.Attribute Msg
myOnTouchDown =
    Element.htmlAttribute <| 
        Html.Events.Extra.Pointer.onWithOptions 
            "touchstart" 
            { stopPropagation = True, preventDefault = True }
            (.pointer >> .pagePos >> TouchStart)

myOnTouchUp : Element.Attribute Msg
myOnTouchUp =
    Element.htmlAttribute <|
        Html.Events.Extra.Pointer.onWithOptions 
            "touchend" 
            { stopPropagation = True, preventDefault = True } 
            (.pointer >> .pagePos >> TouchEnd)

responsiveText : Element.DeviceClass -> Element.Attribute msg
responsiveText class =
    Element.Font.size <|
    if class == Element.BigDesktop then 32
    else 16

view : Model -> Browser.Document Msg
view model =
    { title = "Sliding tiles"
    , body =
        [ Element.layout [myOnPointerDown, myOnTouchDown, myOnPointerUp, myOnTouchUp, responsiveText model.deviceClass] <|
            Element.el
                [ Element.width Element.fill
                , Element.onLeft <| viewSidebar model.deviceClass model.sidebar
                ]
                (case model.game of
                    Nothing ->
                        Element.paragraph []
                            [ Element.text "Generate a board to start! If you've already clicked one of the generator buttons and it's taking some time, you may need to refresh the page and try a smaller board." ]

                    Just ( game, seed ) ->
                        Element.column [ Element.spacing 5, Element.width Element.fill]
                            [ (String.fromInt >> (++) "Seed: " >> Element.text >> Element.el [ Element.centerX, Element.alignTop ]) seed
                            , Sliding.view [ Element.centerX, Element.centerY ] game
                            , if Sliding.won game then
                                Element.el [ Element.centerX, Element.scale 2.0 ] <| Element.text "You win!"

                              else 
                                Element.el [ Element.centerX] <| Element.text <| 
                                    case model.deviceClass of
                                        Element.Desktop -> "Desktop" 
                                        Element.BigDesktop -> "BigDekstop"
                                        Element.Phone -> "Phone"
                                        Element.Tablet -> "Tablet"
                            ]
                )
        ]
    }


viewSidebar : Element.DeviceClass -> SidebarModel -> Element Msg
viewSidebar deviceClass { nextSeed, nextSize, visible } =
    let
        ( nextHeight, nextWidth ) =
            nextSize

        controlsWidth = if deviceClass == Element.BigDesktop then 353 else 222

        visibilityToggle = Element.Input.button [ Element.moveRight controlsWidth ] { onPress = Just <| Sidebar ToggleVisible, label = Element.text <| if visible then "<<" else ">>" }
    in
    if not visible then
        visibilityToggle
    else
        Element.column
            [ Element.alignLeft
            , Element.Border.width 1
            , Element.spacing 10
            , Element.width <| Element.px controlsWidth
            , Element.moveRight controlsWidth
            , Element.onLeft <| visibilityToggle
            ]
        <|
            [ viewControls
            , Element.text "Next game:"
            , Element.row [] [ numberInput "Height" (Sidebar << ChangeHeight) nextHeight, numberInput "Width" (Sidebar << ChangeWidth) nextWidth ]
            ]
                ++ (if nextHeight > 1 && nextWidth > 1 then
                        [ Element.Input.button [ Element.centerX, Element.Border.width 1 ]
                            { onPress = Just GenerateRandomBoard
                            , label = Element.text "Generate random board!"
                            }
                        , numberInput "Seed" (Sidebar << ChangeSeed) nextSeed
                        , Element.Input.button [ Element.centerX, Element.Border.width 1 ]
                            { onPress =
                                if nextHeight > 1 && nextWidth > 1 then
                                    Just GenerateSeededBoard

                                else
                                    Nothing
                            , label = Element.text "Generate from seed!"
                            }
                        ]

                    else
                        [ Element.paragraph [] [ Element.text "To guarantee a solvable board, width and height must both be greater than one." ] ]
                   )


viewControls : Element msg
viewControls =
    Element.column []
        [ Element.text "Controls:"
        , Element.paragraph [Element.Font.size 14, Element.paddingXY 20 0]
            [ Element.text "WASD,"
            , Element.text "HJKL,"
            , Element.text "↑←↓→,"
            , Element.text "and tap/click dragging"
            ]
        ]


numberInput : String -> (Int -> Msg) -> Int -> Element Msg
numberInput label tag value =
    let
        tagger =
            String.toInt >> Maybe.withDefault value >> tag
    in
    Element.Input.text []
        { onChange = tagger
        , text = String.fromInt value
        , placeholder = Nothing
        , label = Element.Input.labelAbove [] (Element.text label)
        }
