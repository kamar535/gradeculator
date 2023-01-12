module Main exposing (..)

-- TO READ: https://thoughtbot.com/blog/running-out-of-maps

import Array
import Array.Extra
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, br, i)
import List
import Maybe.Extra
import Round
import String
import Tuple2


type Grade
    = Fail
    | Three
    | Four
    | Five


gradeToString : Grade -> String
gradeToString grade =
    case grade of
        Fail ->
            "F"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"


type alias Module =
    { code : String
    , name : String
    , credits : Int
    , grades : Grades
    }


type Grades
    = SingleGrade Grade
    | Grades (Array.Array ModulePart)


type alias ModulePart =
    { name : String
    , grade : Grade
    }


gradeToMaybeFloat : Grade -> Maybe Float
gradeToMaybeFloat grade =
    case grade of
        Fail ->
            Nothing

        Three ->
            Just 3

        Four ->
            Just 4

        Five ->
            Just 5


moduleWeightAndAveragageGradeFromIndex : Int -> Array.Array Module -> Maybe ( Int, Maybe Float )
moduleWeightAndAveragageGradeFromIndex moduleIndex modules =
    Array.get moduleIndex modules
        |> Maybe.map (\m -> ( m.credits, averageModuleGrade m ))


viewModuleAverageWeightedGrade : Int -> Array.Array Module -> Element Msg
viewModuleAverageWeightedGrade moduleIndex modules =
    case moduleWeightAndAveragageGradeFromIndex moduleIndex modules of
        Nothing ->
            text <| "Error: Invalid module index " ++ String.fromInt moduleIndex

        Just ( _, Nothing ) ->
            text "No grade"

        Just ( w, Just avg ) ->
            text <| String.fromInt <| w * round avg


viewAverageModuleGrade : Int -> Array.Array Module -> Element Msg
viewAverageModuleGrade moduleIndex modules =
    case averageModuleGradeFromIndex moduleIndex modules of
        Nothing ->
            text <| "Error: Invalid module index " ++ String.fromInt moduleIndex

        Just Nothing ->
            text "No grade"

        Just (Just avg) ->
            text <| (String.fromInt <| round avg) ++ " (" ++ Round.round 2 avg ++ ")"


averageModuleGradeFromIndex : Int -> Array.Array Module -> Maybe (Maybe Float)
averageModuleGradeFromIndex moduleIndex modules =
    Array.get moduleIndex modules |> Maybe.map averageModuleGrade


averageModuleGrade : Module -> Maybe Float
averageModuleGrade m =
    averageGrade m.grades


averageGrade : Grades -> Maybe Float
averageGrade grades =
    case grades of
        SingleGrade g ->
            gradeToMaybeFloat g

        Grades gs ->
            Array.toList gs
                |> List.map .grade
                |> Maybe.Extra.traverse gradeToMaybeFloat
                |> Maybe.map List.sum
                |> Maybe.map (\tot -> tot / toFloat (Array.length gs))


finalGrade : Array.Array { a | credits : Int, grades : Grades } -> Maybe ( Int, Int )
finalGrade modules =
    Array.toList modules
        |> List.map (\mod -> ( .credits mod, .grades mod ) |> Tuple2.maybeMapSecond averageGrade)
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl (\( w, g ) ( wSum, gSum ) -> ( w + wSum, (toFloat w * g) + gSum )) ( 0, 0 ))
        |> Maybe.map (\( totalWeight, sum ) -> ( totalWeight, round sum ))


defaultGrade : Grade
defaultGrade =
    Three


initPart : String -> ModulePart
initPart n =
    { name = n
    , grade = defaultGrade
    }


singleGrade : Grades
singleGrade =
    SingleGrade defaultGrade


multipleGrades : List String -> Grades
multipleGrades names =
    Grades <| initModuleParts names



--- OSPP Modules


a1 : Module
a1 =
    { code = "2000"
    , name = "Assignments (part 1)"
    , credits = 2
    , grades = multipleGrades [ "Fundamental concepts", "The process concept", "Threads, synchronization and deadlock" ]
    }


e : Module
e =
    { code = "3000"
    , name = "Written exam (part 1)"
    , credits = 1
    , grades = singleGrade
    }


a2 : Module
a2 =
    { code = "5000"
    , name = "Assignments (part 2)"
    , credits = 1
    , grades = singleGrade
    }


pg : Module
pg =
    { code = "6000"
    , name = "Project (group)"
    , credits = 2
    , grades = singleGrade
    }


pi : Module
pi =
    { code = "7000"
    , name = "Project (individual)"
    , credits = 5
    , grades = singleGrade
    }


osppModules : Array.Array Module
osppModules =
    Array.fromList [ a1, e, a2, pg, pi ]



--- DSP Modules
--- OSPP Modules


dspAssignments : Module
dspAssignments =
    { code = "2010"
    , name = "Assignments"
    , credits = 5
    , grades = multipleGrades [ "Fundamental concepts", "The process concept", "Threads, synchronization and deadlock" ]
    }


dspExam : Module
dspExam =
    { code = "1010"
    , name = "Written exam"
    , credits = 2
    , grades = singleGrade
    }


dspProjectGroup : Module
dspProjectGroup =
    { code = "3020"
    , name = "Project (group)"
    , credits = 2
    , grades = singleGrade
    }


dspProjectIndividual : Module
dspProjectIndividual =
    { code = "3010"
    , name = "Project (individual)"
    , credits = 5
    , grades = singleGrade
    }


dspModules =
    Array.fromList [ dspExam, dspAssignments, dspProjectIndividual, dspProjectGroup ]



---


initModuleParts : List String -> Array.Array ModulePart
initModuleParts names =
    Array.fromList <| List.map initPart names


type alias Model =
    Array.Array Module


type ButtonPosition
    = First
    | Middle
    | Last


type Msg
    = SetModuleGrade Int Grade
    | SetPartGrade Int Int Grade


viewFinalGrade : Array.Array { a | credits : Int, grades : Grades } -> Element Msg
viewFinalGrade model =
    finalGrade model
        |> Maybe.map
            (\( totalWeight, sum ) ->
                let
                    avgGrade =
                        toFloat sum / toFloat totalWeight
                in
                paragraph [ Font.alignLeft ]
                    [ el [ Font.bold ] <| text "Final grade "
                    , text <| " = "
                    , el [ Font.bold ] <| text <| String.fromInt (round avgGrade)
                    , text <| " (" ++ String.fromInt sum
                    , text <|
                        "/"
                            ++ String.fromInt totalWeight
                            ++ " ≈ "
                            ++ Round.round 2 avgGrade
                            ++ ")"
                    ]
            )
        |> Maybe.withDefault (paragraph [ Font.alignLeft, Font.bold ] [ text "No final grade" ])


averageGradeToString : Maybe Float -> String
averageGradeToString avg =
    avg |> Maybe.map (\grade -> String.fromInt (round grade) ++ " (" ++ Round.round 2 grade ++ ")") |> Maybe.withDefault "No Grade"


view : Model -> Html Msg
view model =
    layout [ padding 20 ] <|
        column [ spacing 20, Font.size 12 ]
            [ modulesTable model
            , viewFinalGrade model
            ]


type GradeIndex
    = ModuleIndex Int
    | ModuleAndPartIndex Int Int


getSingleGrade : Grades -> Maybe Grade
getSingleGrade grade =
    case grade of
        SingleGrade g ->
            Just g

        Grades _ ->
            Nothing


getGrades : Grades -> Maybe (Array.Array ModulePart)
getGrades grades =
    case grades of
        Grades gs ->
            Just gs

        SingleGrade _ ->
            Nothing


gradeButtons : GradeIndex -> Array.Array Module -> Element Msg
gradeButtons gradeIndex modules =
    let
        option : Grade -> ButtonPosition -> Input.Option Grade msg
        option g position =
            Input.optionWith g (gradeButton g position (gradeToString g))

        grade : Maybe Grade
        grade =
            case gradeIndex of
                ModuleIndex i ->
                    Maybe.map .grades (Array.get i modules)
                        |> Maybe.andThen getSingleGrade

                ModuleAndPartIndex m p ->
                    Array.get m modules
                        |> Maybe.map .grades
                        |> Maybe.andThen getGrades
                        |> Maybe.andThen (Array.get p)
                        |> Maybe.map .grade

        msg =
            case gradeIndex of
                ModuleIndex moduleIndex ->
                    SetModuleGrade moduleIndex

                ModuleAndPartIndex moduleIndex partIndex ->
                    SetPartGrade moduleIndex partIndex
    in
    Input.radioRow
        [ Border.rounded 6
        ]
        { onChange = msg
        , selected = grade
        , label =
            Input.labelHidden "Select grade"
        , options =
            [ option Fail First
            , option Three Middle
            , option Four Middle
            , option Five Last
            ]
        }


gradeButton : Grade -> ButtonPosition -> String -> Input.OptionState -> Element msg
gradeButton grade position label state =
    let
        r =
            4

        b =
            1

        borders =
            case position of
                First ->
                    { left = b, right = b, top = b, bottom = b }

                Middle ->
                    { left = 0, right = b, top = b, bottom = b }

                Last ->
                    { left = 0, right = b, top = b, bottom = b }

        corners =
            case position of
                First ->
                    { topLeft = r, bottomLeft = r, topRight = 0, bottomRight = 0 }

                Middle ->
                    { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }

                Last ->
                    { topLeft = 0, bottomLeft = 0, topRight = r, bottomRight = r }
    in
    el
        [ paddingEach { left = 6, right = 6, top = 3, bottom = 3 }
        , Font.size 12
        , Border.roundEach corners
        , Border.widthEach borders
        , Border.color color.gray
        , Background.color <|
            if state == Input.Selected then
                case grade of
                    Fail ->
                        color.red

                    _ ->
                        color.green

            else
                color.white
        , Font.color <|
            if state == Input.Selected then
                color.white

            else
                color.black
        ]
    <|
        el [ centerX, centerY ] <|
            text label


{-| TODO: Add module : Int and part : Int fields to the Single and Part records
and module : Int to the Header record in order to
index into the model grades array.
-}
type Row
    = Header { code : String, credits : Int, name : String, moduleIndex : Int }
    | Single { code : String, credits : Int, name : String, moduleIndex : Int }
    | Part { name : String, moduleIndex : Int, partIndex : Int }
    | Sum


getNameAndGrade : Int -> Array.Array ModulePart -> ( String, Grade )
getNameAndGrade index parts =
    case Array.get index parts of
        Nothing ->
            ( "Unknown", Three )

        Just part ->
            ( part.name, part.grade )



-- NOTE: Using Array map (or indexedMap) there is no need to deal with Maybe
-- values when accessing the elements of the array.


moduleToRows : Int -> Module -> List Row
moduleToRows moduleIndex m =
    case m.grades of
        SingleGrade _ ->
            [ Single { code = m.code, credits = m.credits, name = m.name, moduleIndex = moduleIndex } ]

        Grades grades ->
            Header { code = m.code, credits = m.credits, name = m.name, moduleIndex = moduleIndex }
                :: Array.Extra.indexedMapToList
                    (\i part ->
                        Part { name = part.name, moduleIndex = moduleIndex, partIndex = i }
                    )
                    grades


modulesToRows : Array.Array Module -> List Row
modulesToRows ms =
    (Array.Extra.indexedMapToList
        (\index mod -> moduleToRows index mod)
        ms
        |> List.concat
    )
        ++ [ Sum ]


cell : Attribute msg -> Element msg -> Element msg
cell alignment content =
    el [ alignment, centerY, paddingXY 20 10 ] content


tableCell : List (Attribute msg) -> Element msg -> Element msg
tableCell attributes child =
    el
        [ height fill
        , paddingXY 20 6
        ]
    <|
        el (attributes ++ [ alignTop ]) child


sumCell : List (Attribute msg) -> Element msg -> Element msg
sumCell attributes child =
    el
        [ height fill
        , Border.widthEach { top = 2, bottom = 0, left = 0, right = 0 }
        , paddingXY 0 10
        , centerX
        ]
    <|
        el (attributes ++ [ alignBottom ]) child


codeColumn : Row -> Element msg
codeColumn row =
    case row of
        Single record ->
            .code record |> text |> tableCell [ centerX ]

        Header record ->
            .code record |> text |> tableCell [ centerX ]

        Part _ ->
            none

        Sum ->
            sumCell [] none


weightColumn : Array.Array Module -> Row -> Element msg
weightColumn modules row =
    case row of
        Single record ->
            .credits record |> String.fromInt |> text |> tableCell [ centerX ]

        Header record ->
            .credits record |> String.fromInt |> text |> tableCell [ centerX ]

        Part _ ->
            none

        Sum ->
            let
                totalWeight =
                    Array.foldl (\m sumW -> m.credits + sumW) 0 modules
            in
            sumCell [ centerX ] <| text <| String.fromInt <| totalWeight


type alias PaddingEach =
    { left : Int
    , right : Int
    , top : Int
    , bottom : Int
    }


zeroPadding : PaddingEach
zeroPadding =
    { left = 0, right = 0, top = 0, bottom = 0 }


nameColumn : Row -> Element msg
nameColumn r =
    case r of
        Single record ->
            .name record |> text |> tableCell []

        Header record ->
            paragraph [ Font.alignLeft ] [ .name record |> text ] |> tableCell []

        Part record ->
            row []
                [ el [ width (px 20) ] none
                , paragraph [ Font.alignLeft ] [ .name record |> text ]
                ]
                |> tableCell []

        Sum ->
            sumCell [] none


gradeColumn : Array.Array Module -> Row -> Element Msg
gradeColumn modules r =
    case r of
        Single mod ->
            tableCell [ centerX ] <| gradeButtons (ModuleIndex mod.moduleIndex) modules

        Header h ->
            tableCell [ centerX ] <| viewAverageModuleGrade h.moduleIndex modules

        Part part ->
            tableCell [ centerX ] <| gradeButtons (ModuleAndPartIndex part.moduleIndex part.partIndex) modules

        Sum ->
            sumCell [ Font.bold, alignRight ] <| text "Sum"


weightedColumn : Array.Array Module -> Row -> Element Msg
weightedColumn modules w =
    case w of
        Single s ->
            tableCell [ centerX ] <| viewModuleAverageWeightedGrade s.moduleIndex modules

        Header h ->
            tableCell [ centerX ] <| viewModuleAverageWeightedGrade h.moduleIndex modules

        Part _ ->
            none

        Sum ->
            let
                sum =
                    finalGrade modules
                        |> Maybe.map (\( _, s ) -> String.fromInt s)
                        |> Maybe.withDefault "Incomplete"
            in
            sumCell [ centerX ] <| text sum


modulesTable : Array.Array Module -> Element Msg
modulesTable modules =
    let
        headerCell : List (Attribute msg) -> Element msg -> Element msg
        headerCell attributes child =
            -- Same row height for all table columns using elm-ui
            -- https://stackoverflow.com/questions/69364693/same-row-height-for-all-table-columns-using-elm-ui
            el
                [ height fill
                , Font.bold
                , Border.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
                , paddingXY 20 6
                ]
            <|
                el (attributes ++ [ alignBottom ]) child

        centeredStrings strings =
            List.intersperse (html <| br [] []) (List.map text strings) |> paragraph []
    in
    table []
        { data = modulesToRows modules
        , columns =
            [ { header = headerCell [ centerX ] <| text "Code"
              , width = shrink
              , view = codeColumn
              }
            , { header = headerCell [] <| text "Ladok module"
              , width = fill
              , view = nameColumn
              }
            , { header = headerCell [ centerX ] <| text "Grade"
              , width = shrink
              , view = gradeColumn modules
              }
            , { header = headerCell [ Font.center ] <| centeredStrings [ "Weight", "(credits)" ]
              , width = shrink
              , view = weightColumn modules
              }
            , { header = headerCell [ Font.center ] <| centeredStrings [ "Weighted", "grade" ]
              , width = shrink
              , view = weightedColumn modules
              }
            ]
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetModuleGrade moduleIndex grade ->
            Array.Extra.update moduleIndex
                (\mod -> { mod | grades = SingleGrade grade })
                model

        SetPartGrade moduleIndex partIndex grade ->
            Array.Extra.update moduleIndex
                (\mod ->
                    case mod.grades of
                        SingleGrade _ ->
                            mod

                        Grades grades ->
                            { mod
                                | grades =
                                    Grades
                                        (Array.Extra.update
                                            partIndex
                                            (\part -> { part | grade = grade })
                                            grades
                                        )
                            }
                )
                model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = osppModules
        , view = view
        , update = update
        }


color =
    { blue = rgb255 0x72 0x9F 0xCF
    , red = rgb255 255 0 0
    , green = rgb255 0 128 0
    , black = rgb255 0 0 0
    , gray = rgb255 0x7F 0x7F 0x7F
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    }
