module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, a)
import Maybe.Extra
import Round
import Tuple.Extra



---- MODEL ----


type alias Model =
    Array Module


type alias Module =
    { code : String
    , name : String
    , credits : Int
    , assignments : Assignments
    }


type Assignments
    = SingleGrade345 (Maybe Grade345)
    | AverageGrade345 (Array Grade345Assignment)
    | FailPassPoints FailPassPointsAssignments
    | PointsOnly FailPassPointsAssignments


type Grade345
    = Three
    | Four
    | Five


type alias Grade345Assignment =
    { name : String
    , grade : Maybe Grade345
    }


type alias FailPassPointsAssignments =
    { four : Int
    , five : Int
    , assignments : Array PointsAssignment
    }


type alias PointsAssignment =
    { name : String
    , mandatoryPass : Bool
    , max : Int
    , points : Int
    }


type ModuleGrade
    = SingleModuleGrade (Maybe Grade345)
    | AverageModuleGrade (Maybe { grade : Grade345, average : Float })
    | FailPassPointsModuleGrade
        { grade : Maybe Grade345
        , points : Int
        , four : Int
        , five : Int
        , max : Int
        }
    | PointsOnlyModuleGrade
        { grade : Maybe Grade345
        , points : Int
        , four : Int
        , five : Int
        , max : Int
        }



---- Functions ----


numberOfAssignments : Module -> Int
numberOfAssignments m =
    case m.assignments of
        AverageGrade345 a ->
            Array.length a

        FailPassPoints a ->
            Array.length a.assignments

        PointsOnly a ->
            Array.length a.assignments

        _ ->
            0


moduleGradeToMaybeInt : ModuleGrade -> Maybe Int
moduleGradeToMaybeInt grade =
    case grade of
        SingleModuleGrade g ->
            g |> Maybe.map grade345ToInt

        AverageModuleGrade (Just g) ->
            Just <| grade345ToInt g.grade

        AverageModuleGrade Nothing ->
            Nothing

        FailPassPointsModuleGrade g ->
            g.grade |> Maybe.map grade345ToInt

        PointsOnlyModuleGrade g ->
            g.grade |> Maybe.map grade345ToInt


singleGrade345 : Assignments
singleGrade345 =
    SingleGrade345 (Just Three)


averageGrade345 : List String -> Assignments
averageGrade345 names =
    names
        |> List.map (\name -> { name = name, grade = Just Three })
        |> Array.fromList
        |> AverageGrade345


higherGradeAssignment : String -> Int -> PointsAssignment
higherGradeAssignment name max =
    { name = name
    , mandatoryPass = True
    , max = max
    , points = 0
    }


pointsOnlyAssignment : String -> Int -> PointsAssignment
pointsOnlyAssignment name max =
    { name = name
    , mandatoryPass = True
    , max = max
    , points = 1
    }


higherGradeAssignments : Int -> Int -> List ( String, Int ) -> Assignments
higherGradeAssignments four five assignments =
    FailPassPoints
        { four = four
        , five = five
        , assignments =
            assignments
                |> List.map (\( name, max ) -> higherGradeAssignment name max)
                |> Array.fromList
        }


pointsOnlyAssignments : Int -> Int -> List ( String, Int ) -> Assignments
pointsOnlyAssignments four five assignments =
    PointsOnly
        { four = four
        , five = five
        , assignments =
            assignments
                |> List.map (\( name, max ) -> pointsOnlyAssignment name max)
                |> Array.fromList
        }


moduleGrade : Module -> ModuleGrade
moduleGrade m =
    case m.assignments of
        SingleGrade345 maybeGrade345 ->
            SingleModuleGrade maybeGrade345

        AverageGrade345 assignments ->
            averageGrade assignments

        FailPassPoints assignments ->
            pointsGrade assignments

        PointsOnly assignments ->
            pointsOnlyGrade assignments (numberOfAssignments m)


averageGradeHelper : Array Grade345Assignment -> Maybe { grade : Grade345, average : Float }
averageGradeHelper assignments =
    assignments
        |> Array.map .grade
        |> Array.toList
        |> Maybe.Extra.combine
        |> Maybe.map
            (\grades ->
                List.map grade345ToInt grades
                    |> List.sum
                    |> (\sum -> toFloat sum / toFloat (Array.length assignments))
            )
        |> Maybe.andThen
            (\average ->
                ( intToGrade345 (round average), Just average ) |> Tuple.Extra.sequenceMaybe
            )
        |> Maybe.map (\( grade, average ) -> { grade = grade, average = average })


averageGrade : Array Grade345Assignment -> ModuleGrade
averageGrade assignments =
    assignments
        |> averageGradeHelper
        |> AverageModuleGrade


pointsToGrade345 : Int -> Int -> Int -> Grade345
pointsToGrade345 points four five =
    if points >= five then
        Five

    else if points >= four then
        Four

    else
        Three


pointsGradeHelper :
    FailPassPointsAssignments
    ->
        { grade : Maybe Grade345
        , points : Int
        , four : Int
        , five : Int
        , max : Int
        }
pointsGradeHelper assignments =
    assignments.assignments
        |> Array.map (\assignment -> ( assignment.mandatoryPass, assignment.points, assignment.max ))
        |> Array.foldl
            (combine3Tuples
                (&&)
                (+)
                (+)
            )
            ( True, 0, 0 )
        |> (\( allMandatoryPass, points, max ) ->
                let
                    grade =
                        if allMandatoryPass then
                            Just <| pointsToGrade345 points assignments.four assignments.five

                        else
                            Nothing
                in
                { grade = grade, points = points, max = max, four = assignments.four, five = assignments.five }
           )


pointsOnlyGradeHelper :
    FailPassPointsAssignments
    -> Int
    ->
        { grade : Maybe Grade345
        , points : Int
        , four : Int
        , five : Int
        , max : Int
        }
pointsOnlyGradeHelper assignments passOffset =
    assignments.assignments
        |> Array.map (\assignment -> ( assignment.mandatoryPass, assignment.points, assignment.max ))
        |> Array.foldl
            (combine3Tuples
                (&&)
                (+)
                (+)
            )
            ( True, 0, 0 )
        |> (\( allMandatoryPass, points, max ) ->
                let
                    grade =
                        if allMandatoryPass then
                            Just <| pointsToGrade345 (points - passOffset) assignments.four assignments.five

                        else
                            Nothing
                in
                { grade = grade, points = points, max = max, four = assignments.four, five = assignments.five }
           )


pointsGrade : FailPassPointsAssignments -> ModuleGrade
pointsGrade assignments =
    FailPassPointsModuleGrade <| pointsGradeHelper assignments


pointsOnlyGrade : FailPassPointsAssignments -> Int -> ModuleGrade
pointsOnlyGrade assignments passOffset =
    PointsOnlyModuleGrade <| pointsOnlyGradeHelper assignments passOffset


grade345ToInt : Grade345 -> Int
grade345ToInt grade =
    case grade of
        Three ->
            3

        Four ->
            4

        Five ->
            5


intToGrade345 : Int -> Maybe Grade345
intToGrade345 grade =
    case grade of
        3 ->
            Just Three

        4 ->
            Just Four

        5 ->
            Just Five

        _ ->
            Nothing


tupleMap3 : (a -> x) -> (b -> y) -> (c -> z) -> ( a, b, c ) -> ( x, y, z )
tupleMap3 f g h ( a, b, c ) =
    ( f a, g b, h c )


combine3Tuples : (a -> a -> x) -> (b -> b -> y) -> (c -> c -> z) -> ( a, b, c ) -> ( a, b, c ) -> ( x, y, z )
combine3Tuples f g h ( a, b, c ) ( aa, bb, cc ) =
    ( f a aa, g b bb, h c cc )


type alias FinalGrade =
    { finalGrade : Maybe Float, credits : Int, weightedGrades : Maybe Int }


finalGrade : Array Module -> FinalGrade
finalGrade modules =
    let
        credits =
            Array.map .credits modules |> Array.toList |> List.sum

        weightedGrades =
            Array.map (\m -> ( m.credits, moduleGrade m |> moduleGradeToMaybeInt )) modules
                |> Array.toList
                |> List.map Tuple.Extra.sequenceSecondMaybe
                |> Maybe.Extra.combine
                |> Maybe.map (List.map (\( w, c ) -> w * c))
                |> Maybe.map List.sum

        fg =
            Maybe.map toFloat weightedGrades
                |> Maybe.map (\sum -> sum / toFloat credits)
    in
    { finalGrade = fg, credits = credits, weightedGrades = weightedGrades }



---- Helper functions ----


assignmentsPart1Average : Module
assignmentsPart1Average =
    { code = "2000"
    , name = "Assignments (part 1, average)"
    , credits = 2
    , assignments =
        averageGrade345
            [ "Fundamental concepts"
            , "The process concept"
            , "Threads, synchronization and deadlock"
            ]
    }


assignmentsPart1Points : Module
assignmentsPart1Points =
    { code = "2000"
    , name = "Assignments (part 1)"
    , credits = 2
    , assignments =
        higherGradeAssignments 4
            8
            [ ( "Fundamental concepts", 3 )
            , ( "The process concept", 3 )
            , ( "Threads, synchronization and deadlock", 4 )
            ]
    }


dspAssignments : Module
dspAssignments =
    { code = "2010"
    , name = "Assignments"
    , credits = 5
    , assignments =
        higherGradeAssignments 4
            8
            [ ( "Fundamental concepts", 3 )
            , ( "The process concept", 3 )
            , ( "Threads, synchronization and deadlock", 4 )
            ]
    }


dspAssignmentsV2 : Module
dspAssignmentsV2 =
    { code = "2010"
    , name = "Assignments"
    , credits = 5
    , assignments =
        pointsOnlyAssignments 4
            8
            [ ( "Fundamental concepts", 3 )
            , ( "The process concept", 3 )
            , ( "Threads, synchronization and deadlock", 4 )
            ]
    }


writtenExam : Module
writtenExam =
    { code = "3000"
    , name = "Written exam (part 1)"
    , credits = 1
    , assignments = singleGrade345
    }


dspWrittenExam : Module
dspWrittenExam =
    { code = "1010"
    , name = "Written exam"
    , credits = 2
    , assignments = singleGrade345
    }


assignmentsPart2 : Module
assignmentsPart2 =
    { code = "5000"
    , name = "Assignments (part 2)"
    , credits = 1
    , assignments = singleGrade345
    }


projectGroup : Module
projectGroup =
    { code = "6000"
    , name = "Project (group)"
    , credits = 2
    , assignments = singleGrade345
    }


dspProjectGroup : Module
dspProjectGroup =
    { code = "3020"
    , name = "Project (group)"
    , credits = 2
    , assignments = singleGrade345
    }


projectIndividual : Module
projectIndividual =
    { code = "7000"
    , name = "Project (individual)"
    , credits = 5
    , assignments = singleGrade345
    }


dspProjectIndividual : Module
dspProjectIndividual =
    { code = "3010"
    , name = "Project (individual)"
    , credits = 5
    , assignments = singleGrade345
    }


testModel : Model
testModel =
    Array.fromList
        [ assignmentsPart1Average
        , assignmentsPart1Points
        , writtenExam
        , assignmentsPart2
        , projectGroup
        , projectIndividual
        ]


osppModel : Model
osppModel =
    Array.fromList
        [ assignmentsPart1Points
        , writtenExam
        , assignmentsPart2
        , projectGroup
        , projectIndividual
        ]


dspModel : Model
dspModel =
    Array.fromList
        [ dspWrittenExam
        , dspAssignments
        , dspProjectIndividual
        , dspProjectGroup
        ]


dspModelV2 : Model
dspModelV2 =
    Array.fromList
        [ dspWrittenExam
        , dspAssignmentsV2
        , dspProjectIndividual
        , dspProjectGroup
        ]


init : ( Model, Cmd Msg )
init =
    ( dspModelV2, Cmd.none )


getModuleGrade345 : Module -> Maybe Grade345
getModuleGrade345 m =
    let
        grade =
            moduleGrade m
    in
    case grade of
        SingleModuleGrade g ->
            g

        _ ->
            Nothing



---- UPDATE ----


type Msg
    = SetSinglGrade345 Int (Maybe Grade345)
    | SetAverageGrade345 Int Int (Maybe Grade345)
    | SetMandatoryPass Int Int Bool
    | SetFailPassPointsPointsGradePoints Int Int Int
    | SetPointsOnlyPoints Int Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSinglGrade345 moduleIndex grade ->
            let
                newModel =
                    Array.Extra.update moduleIndex (updateSingleGrade grade) model
            in
            ( newModel, Cmd.none )

        SetAverageGrade345 moduleIndex assignmentIndex grade ->
            let
                newModel =
                    Array.Extra.update moduleIndex (updateAssignmentGrade assignmentIndex grade) model
            in
            ( newModel, Cmd.none )

        SetMandatoryPass moduleIndex assignmentIndex pass ->
            let
                newModel =
                    Array.Extra.update moduleIndex (updateMandatoryPass assignmentIndex pass) model
            in
            ( newModel, Cmd.none )

        SetFailPassPointsPointsGradePoints moduleIndex assignmentIndex points ->
            let
                newModel =
                    Array.Extra.update moduleIndex (updateHigherGradePoints assignmentIndex points) model
            in
            ( newModel, Cmd.none )

        SetPointsOnlyPoints moduleIndex assignmentIndex points ->
            let
                newModel =
                    Array.Extra.update moduleIndex (updateHigherGradePoints assignmentIndex points) model
            in
            ( newModel, Cmd.none )


updateSingleGrade : Maybe Grade345 -> Module -> Module
updateSingleGrade grade m =
    case m.assignments of
        SingleGrade345 _ ->
            { m | assignments = SingleGrade345 grade }

        _ ->
            m


updateMandatoryPass : Int -> Bool -> Module -> Module
updateMandatoryPass assignmentIndex pass m =
    case m.assignments of
        FailPassPoints assignments ->
            let
                newAssignments : Array PointsAssignment
                newAssignments =
                    Array.Extra.update assignmentIndex (\a -> { a | mandatoryPass = pass }) assignments.assignments
            in
            { m | assignments = FailPassPoints { assignments | assignments = newAssignments } }

        _ ->
            m


updateHigherGradePoints : Int -> Int -> Module -> Module
updateHigherGradePoints assignmentIndex points m =
    case m.assignments of
        FailPassPoints assignments ->
            let
                newAssignments : Array PointsAssignment
                newAssignments =
                    Array.Extra.update assignmentIndex (\a -> { a | points = points }) assignments.assignments
            in
            { m | assignments = FailPassPoints { assignments | assignments = newAssignments } }

        PointsOnly assignments ->
            case points of
                0 ->
                    let
                        newAssignments : Array PointsAssignment
                        newAssignments =
                            Array.Extra.update assignmentIndex
                                (\a ->
                                    { a
                                        | mandatoryPass = False
                                        , points = points
                                    }
                                )
                                assignments.assignments
                    in
                    { m | assignments = PointsOnly { assignments | assignments = newAssignments } }

                _ ->
                    let
                        newAssignments : Array PointsAssignment
                        newAssignments =
                            Array.Extra.update assignmentIndex
                                (\a ->
                                    { a
                                        | mandatoryPass = True
                                        , points = points
                                    }
                                )
                                assignments.assignments
                    in
                    { m | assignments = PointsOnly { assignments | assignments = newAssignments } }

        _ ->
            m


updateAssignmentGrade : Int -> Maybe Grade345 -> Module -> Module
updateAssignmentGrade assignmentIndex grade m =
    case m.assignments of
        AverageGrade345 assignments ->
            let
                newAssignments =
                    Array.Extra.update assignmentIndex (\a -> { a | grade = grade }) assignments
            in
            { m | assignments = AverageGrade345 newAssignments }

        _ ->
            m



---- VIEW ----


grade345ToWord : Grade345 -> String
grade345ToWord grade =
    case grade of
        Three ->
            "Three"

        Four ->
            "Four"

        Five ->
            "Five"


viewPointLadder : ( Grade345, List ( Grade345, List ( Int, Input.OptionState ) ) ) -> Element Msg
viewPointLadder ( grade, pl ) =
    row [ spacing 10 ] <|
        List.map (viewPointLadderHelper grade) pl


viewPointsOnlyLadder : ( Grade345, List ( Grade345, List ( Int, Input.OptionState ) ) ) -> Element Msg
viewPointsOnlyLadder ( grade, pl ) =
    row [ spacing 10 ] <|
        List.map (viewPointsOnlyLadderHelper grade) pl


viewPointsOnlyLadderHelper : Grade345 -> ( Grade345, List ( Int, Input.OptionState ) ) -> Element Msg
viewPointsOnlyLadderHelper g ( grade, points ) =
    let
        weight =
            if g == grade then
                Font.bold

            else
                Font.regular

        bgColor state point =
            Background.color <|
                if state == Input.Selected then
                    color.blue

                else
                    color.white
    in
    column [ spacing 10 ]
        [ el [ centerX, Font.size 12, weight ] <| text <| grade345ToWord grade
        , row [] <|
            List.map
                (\( position, ( point, state ) ) ->
                    column [ Font.size 12, spacing 6 ]
                        [ el [ centerX, width (px 14), height (px 8), borders 1 position, corners 3 position, Border.color color.gray, bgColor state point ] <| none
                        , el [ centerX ] <| text <| String.fromInt point
                        ]
                )
                (positions points)
        ]


viewPointLadderHelper : Grade345 -> ( Grade345, List ( Int, Input.OptionState ) ) -> Element Msg
viewPointLadderHelper g ( grade, points ) =
    let
        weight =
            if g == grade then
                Font.bold

            else
                Font.regular

        bgColor state =
            Background.color <|
                if state == Input.Selected then
                    color.blue

                else
                    color.white
    in
    column [ spacing 10 ]
        [ el [ centerX, Font.size 12, weight ] <| text <| grade345ToWord grade
        , row [] <|
            List.map
                (\( position, ( point, state ) ) ->
                    column [ Font.size 12, spacing 6 ]
                        [ el [ centerX, width (px 14), height (px 8), borders 1 position, corners 3 position, Border.color color.gray, bgColor state ] <| none
                        , el [ centerX ] <| text <| String.fromInt point
                        ]
                )
                (positions points)
        ]


pointLadder : Int -> Int -> Int -> Int -> ( Grade345, List ( Grade345, List ( Int, Input.OptionState ) ) )
pointLadder points four five max =
    ( pointsToGrade345 points four five
    , [ ( Three, 0, four - 1 )
      , ( Four, four, five - 1 )
      , ( Five, five, max )
      ]
        |> List.map (\( grade, start, end ) -> ( grade, List.range start end ))
        |> List.map
            (Tuple.mapSecond
                (List.map
                    (\x ->
                        ( x
                        , if points >= x then
                            Input.Selected

                          else
                            Input.Idle
                        )
                    )
                )
            )
    )


viewModuleGrade345 : Maybe { grade : Grade345, average : Float } -> Element Msg
viewModuleGrade345 grade =
    grade
        |> Maybe.map (\g -> grade345ToString (Just g.grade) ++ " (average " ++ Round.round 2 g.average ++ ")" |> text)
        |> Maybe.withDefault (text "No grade")


viewFinalGrade : Model -> Element Msg
viewFinalGrade model =
    let
        fg =
            finalGrade model

        txt =
            ( fg.finalGrade, fg.weightedGrades )
                |> Tuple.Extra.sequenceMaybe
                |> Maybe.map
                    (\( fgrade, wgrades ) ->
                        String.fromInt (round fgrade)
                            ++ "   ("
                            ++ String.fromInt wgrades
                            ++ " / "
                            ++ String.fromInt fg.credits
                            ++ "  ≈  "
                            ++ Round.round 2 fgrade
                            ++ ")"
                    )
                |> Maybe.withDefault "No grade"
    in
    row [] [ el [ Font.bold ] <| text "Final grade   ", text txt ]


type Position
    = Single
    | First
    | Middle
    | Last


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


borders : Int -> Position -> Attribute msg
borders width position =
    Border.widthEach <|
        case position of
            Single ->
                { left = width
                , right = width
                , top = width
                , bottom = width
                }

            First ->
                { left = width
                , right = width
                , top = width
                , bottom = width
                }

            Middle ->
                { left = 0
                , right = width
                , top = width
                , bottom = width
                }

            Last ->
                { left = 0
                , right = width
                , top = width
                , bottom = width
                }


corners : Int -> Position -> Attribute msg
corners radius position =
    Border.roundEach <|
        case position of
            Single ->
                { topLeft = radius
                , bottomLeft = radius
                , topRight = radius
                , bottomRight = radius
                }

            First ->
                { topLeft = radius
                , bottomLeft = radius
                , topRight = 0
                , bottomRight = 0
                }

            Middle ->
                { topLeft = 0
                , bottomLeft = 0
                , topRight = 0
                , bottomRight = 0
                }

            Last ->
                { topLeft = 0
                , bottomLeft = 0
                , topRight = radius
                , bottomRight = radius
                }


button : Position -> Color -> String -> Input.OptionState -> Element Msg
button position c label state =
    el
        [ paddingEach { left = 6, right = 6, top = 3, bottom = 3 }
        , Font.size 12
        , corners 4 position
        , borders 1 position
        , Border.color color.gray
        , Background.color <|
            if state == Input.Selected then
                if label == "F" then
                    color.red

                else
                    c

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


grade345ToString : Maybe Grade345 -> String
grade345ToString grade =
    grade
        |> Maybe.map (grade345ToInt >> String.fromInt)
        |> Maybe.withDefault "F"


grade345ToStringOrNoGrade : Maybe Grade345 -> String
grade345ToStringOrNoGrade grade =
    grade
        |> Maybe.map (grade345ToInt >> String.fromInt)
        |> Maybe.withDefault "No grade"


viewPointsModuleGrade : Maybe Grade345 -> Int -> Int -> String
viewPointsModuleGrade grade points max =
    grade
        |> Maybe.map (grade345ToInt >> String.fromInt)
        |> Maybe.map
            (\s ->
                s
                    ++ " ("
                    ++ String.fromInt points
                    ++ " of "
                    ++ String.fromInt max
                    ++ " points)"
            )
        |> Maybe.withDefault "No grade"


positions : List a -> List ( Position, a )
positions list =
    let
        n =
            List.length list

        indexToPostion i =
            if i == 0 then
                if n == 1 then
                    Single

                else
                    First

            else if i == (n - 1) then
                Last

            else
                Middle
    in
    list
        |> List.indexedMap (\i x -> ( indexToPostion i, x ))


mkOptions : List a -> (a -> String) -> (a -> Color) -> List (Input.Option a Msg)
mkOptions values valueToString c =
    values
        |> positions
        |> List.map (mkOption valueToString c)


mkOption : (a -> String) -> (a -> Color) -> ( Position, a ) -> Input.Option a Msg
mkOption valueToString c ( position, value ) =
    Input.optionWith value <| button position (c value) (valueToString value)


grade345Buttons : Maybe Grade345 -> (Maybe Grade345 -> Msg) -> Element Msg
grade345Buttons selected msg =
    Input.radioRow []
        { onChange = msg
        , selected = Just selected
        , label = Input.labelHidden "Select grade"
        , options = mkOptions [ Nothing, Just Three, Just Four, Just Five ] grade345ToString (\_ -> color.green)
        }


boolToFailPass : Bool -> String
boolToFailPass bool =
    if bool then
        "P"

    else
        "F"


mandatoryPassButtons : Int -> Int -> Bool -> Element Msg
mandatoryPassButtons moduleIndex assignmentIndex selected =
    Input.radioRow []
        { onChange = SetMandatoryPass moduleIndex assignmentIndex
        , selected = Just selected
        , label = Input.labelHidden "Select mandaatory fail or pass"
        , options = mkOptions [ False, True ] boolToFailPass (\_ -> color.green)
        }


higherGradePointsButtons : Int -> Int -> Int -> Int -> Element Msg
higherGradePointsButtons moduleIndex assignmentIndex points selected =
    Input.radioRow []
        { onChange = SetFailPassPointsPointsGradePoints moduleIndex assignmentIndex
        , selected = Just selected
        , label = Input.labelHidden "Select higher grade points"
        , options = mkOptions (List.range 0 points) String.fromInt (\_ -> color.blue)
        }


pointsOnlyButtons : Int -> Int -> Int -> Int -> Element Msg
pointsOnlyButtons moduleIndex assignmentIndex points selected =
    let
        theColor point =
            case point of
                0 ->
                    color.red

                1 ->
                    color.green

                _ ->
                    color.blue
    in
    row [ spacing 10 ]
        [ Input.radioRow []
            { onChange = SetPointsOnlyPoints moduleIndex assignmentIndex
            , selected = Just selected
            , label = Input.labelHidden "Select higher grade points"
            , options =
                mkOptions (List.range 0 (points + 1))
                    String.fromInt
                    theColor
            }
        ]


type TableRow
    = AverageModuleRow
        { code : String
        , name : String
        , grade : Maybe { grade : Grade345, average : Float }
        , credits : Int
        }
    | AverageAssignmentRow
        { name : String
        , moduleIndex : Int
        , assignmentIndex : Int
        , grade : Maybe Grade345
        }
    | FailPassPointsModuleRow
        { code : String
        , name : String
        , grade : Maybe Grade345
        , points : Int
        , four : Int
        , five : Int
        , max : Int
        , credits : Int
        }
    | PointsOnlyModuleRow
        { code : String
        , name : String
        , grade : Maybe Grade345
        , points : Int
        , four : Int
        , five : Int
        , max : Int
        , numberOfAssignments : Int
        , credits : Int
        }
    | FailPassPointsAssignmentRow
        { name : String
        , moduleIndex : Int
        , assignmentIndex : Int
        , mandatoryPass : Bool
        , points : Int
        , max : Int
        }
    | PointsOnlyAssignmentRow
        { name : String
        , moduleIndex : Int
        , assignmentIndex : Int
        , mandatoryPass : Bool
        , points : Int
        , max : Int
        }
    | SingleGrade345ModuleRow
        { code : String
        , name : String
        , moduleIndex : Int
        , grade : Maybe Grade345
        , credits : Int
        }
    | SumRow
        { sumCredits : Int
        , sumWeightedGrades : Maybe Int
        }


moduleTableRows : Int -> Module -> List TableRow
moduleTableRows moduleIndex m =
    case m.assignments of
        SingleGrade345 grade ->
            [ SingleGrade345ModuleRow { code = m.code, moduleIndex = moduleIndex, name = m.name, grade = grade, credits = m.credits } ]

        AverageGrade345 assignments ->
            let
                grade =
                    averageGradeHelper assignments
            in
            AverageModuleRow { code = m.code, name = m.name, grade = grade, credits = m.credits }
                :: (assignments
                        |> Array.toList
                        |> List.indexedMap
                            (\assignmentIndex assignment ->
                                AverageAssignmentRow { name = assignment.name, moduleIndex = moduleIndex, assignmentIndex = assignmentIndex, grade = assignment.grade }
                            )
                   )

        FailPassPoints assignments ->
            let
                grade =
                    pointsGradeHelper assignments
            in
            FailPassPointsModuleRow { code = m.code, name = m.name, grade = grade.grade, points = grade.points, four = assignments.four, five = assignments.five, max = grade.max, credits = m.credits }
                :: (assignments.assignments
                        |> Array.toList
                        |> List.indexedMap
                            (\assignmentIndex assignment ->
                                FailPassPointsAssignmentRow { name = assignment.name, moduleIndex = moduleIndex, assignmentIndex = assignmentIndex, mandatoryPass = assignment.mandatoryPass, points = assignment.points, max = assignment.max }
                            )
                   )

        PointsOnly assignments ->
            let
                grade =
                    pointsOnlyGradeHelper assignments n

                n =
                    numberOfAssignments m
            in
            PointsOnlyModuleRow
                { code = m.code
                , name = m.name
                , grade = grade.grade
                , points = grade.points
                , four = assignments.four + n
                , five = assignments.five + n
                , max = grade.max + n
                , credits = m.credits
                , numberOfAssignments = n
                }
                :: (assignments.assignments
                        |> Array.toList
                        |> List.indexedMap
                            (\assignmentIndex assignment ->
                                PointsOnlyAssignmentRow { name = assignment.name, moduleIndex = moduleIndex, assignmentIndex = assignmentIndex, mandatoryPass = assignment.mandatoryPass, points = assignment.points, max = assignment.max }
                            )
                   )


modelToTableRows : Model -> List TableRow
modelToTableRows model =
    let
        fg1 =
            finalGrade model

        fg =
            SumRow { sumCredits = fg1.credits, sumWeightedGrades = fg1.weightedGrades }
    in
    Array.indexedMap (\moduleIndex m -> moduleTableRows moduleIndex m) model
        |> Array.toList
        |> List.concat
        |> (\rows -> List.append rows [ fg ])


exampleTableRows : List TableRow
exampleTableRows =
    [ AverageModuleRow
        { code = "2000"
        , name = "Assignments (part 1)"
        , grade = Just { grade = Four, average = 4.56 }
        , credits = 2
        }
    , AverageAssignmentRow
        { name = "Fundamental concepts"
        , moduleIndex = 0
        , assignmentIndex = 0
        , grade = Just Four
        }
    , FailPassPointsModuleRow
        { code = "2000"
        , name = "Assignments (part 1)"
        , grade = Just Three
        , points = 7
        , four = 6
        , five = 8
        , max = 10
        , credits = 2
        }
    , FailPassPointsAssignmentRow
        { name = "Fundamental Concepts"
        , moduleIndex = 1
        , assignmentIndex = 0
        , mandatoryPass = True
        , points = 2
        , max = 4
        }
    , SingleGrade345ModuleRow
        { code = "3000"
        , name = "Written exam (part 1)"
        , moduleIndex = 2
        , grade = Just Five
        , credits = 1
        }
    ]


codeColumn row =
    case row of
        AverageModuleRow r ->
            cell [] <| text r.code

        FailPassPointsModuleRow r ->
            cell [] <| text r.code

        SingleGrade345ModuleRow r ->
            cell [] <| text r.code

        SumRow _ ->
            cell [ topBorder ] none

        _ ->
            none


moduleRowPadding =
    paddingEach { top = 10, bottom = 30, left = 10, right = 0 }


nameColumn row =
    case row of
        AverageModuleRow r ->
            cell [ moduleRowPadding ] <| paragraph [] [ text r.name ]

        AverageAssignmentRow r ->
            cell [] <| paragraph [] [ text r.name ]

        FailPassPointsModuleRow r ->
            cell [] <| paragraph [] [ text r.name ]

        PointsOnlyModuleRow r ->
            cell [] <| paragraph [] [ text r.name ]

        FailPassPointsAssignmentRow r ->
            cell [] <| paragraph [] [ text r.name ]

        PointsOnlyAssignmentRow r ->
            cell [] <| paragraph [] [ text r.name ]

        SingleGrade345ModuleRow r ->
            cell [] <| paragraph [] [ text r.name ]

        SumRow _ ->
            cell [ topBorder, height fill ] none


gradeColumn row =
    case row of
        AverageModuleRow r ->
            cell [] <| viewModuleGrade345 r.grade

        AverageAssignmentRow r ->
            cell [] <| grade345Buttons r.grade (SetAverageGrade345 r.moduleIndex r.assignmentIndex)

        FailPassPointsModuleRow r ->
            cell [] <| text <| viewPointsModuleGrade r.grade r.points r.max

        PointsOnlyModuleRow r ->
            cell [] <| text <| viewPointsModuleGrade r.grade r.points r.max

        FailPassPointsAssignmentRow r ->
            cell [] <| mandatoryPassButtons r.moduleIndex r.assignmentIndex r.mandatoryPass

        PointsOnlyAssignmentRow _ ->
            cell [] none

        SingleGrade345ModuleRow r ->
            cell [] <| grade345Buttons r.grade (SetSinglGrade345 r.moduleIndex)

        SumRow _ ->
            cell [ topBorder ] none


pointsColumn row =
    case row of
        FailPassPointsModuleRow r ->
            cell [] <|
                viewPointLadder <|
                    pointLadder r.points r.four r.five r.max

        PointsOnlyModuleRow r ->
            cell [] <|
                viewPointsOnlyLadder <|
                    pointLadder r.points r.four r.five r.max

        FailPassPointsAssignmentRow r ->
            cell [] <| higherGradePointsButtons r.moduleIndex r.assignmentIndex r.max r.points

        PointsOnlyAssignmentRow r ->
            cell [] <| pointsOnlyButtons r.moduleIndex r.assignmentIndex r.max r.points

        SumRow _ ->
            cell [ topBorder, Font.alignRight, Font.bold ] <| text "Sum"

        _ ->
            none


centeredCell child =
    cell [ Font.center ] child


newWeightColumn row =
    case row of
        AverageModuleRow r ->
            centeredCell <| text <| String.fromInt r.credits

        FailPassPointsModuleRow r ->
            centeredCell <| text <| String.fromInt r.credits

        PointsOnlyModuleRow r ->
            centeredCell <| text <| String.fromInt r.credits

        SingleGrade345ModuleRow r ->
            centeredCell <| text <| String.fromInt r.credits

        SumRow r ->
            cell [ topBorder ] <| el [ centerX ] <| text <| String.fromInt r.sumCredits

        _ ->
            none


weightedGrade : Maybe Grade345 -> Int -> String
weightedGrade grade credits =
    grade
        |> Maybe.map
            (\g ->
                g
                    |> grade345ToInt
                    |> (*) credits
                    |> String.fromInt
            )
        |> Maybe.withDefault "No grade"


weightedGradeColumn row =
    case row of
        AverageModuleRow r ->
            centeredCell <|
                text
                    (r.grade
                        |> Maybe.map (\g -> grade345ToInt g.grade * r.credits |> String.fromInt)
                        |> Maybe.withDefault "No grade"
                    )

        FailPassPointsModuleRow r ->
            centeredCell <| text <| weightedGrade r.grade r.credits

        PointsOnlyModuleRow r ->
            centeredCell <| text <| weightedGrade r.grade r.credits

        SingleGrade345ModuleRow r ->
            centeredCell <| text <| weightedGrade r.grade r.credits

        SumRow r ->
            cell [ topBorder ] <| el [ centerX ] <| text <| Maybe.withDefault "Incomplete" <| Maybe.map String.fromInt <| r.sumWeightedGrades

        _ ->
            none


viewTable model =
    modelToTableRows model |> viewTableRows


viewTableRows rows =
    table [ width (px 800) ]
        { data = rows
        , columns =
            [ { header = header "Code"
              , width = px 70
              , view = codeColumn
              }
            , { header = header "Ladok module"
              , width = fill
              , view = nameColumn
              }
            , { header = header "Grade"
              , width = px 150
              , view = gradeColumn
              }
            , { header = header "Points"
              , width = fill |> maximum 100
              , view = pointsColumn
              }
            , { header = centeredHeader "Weight\n(credits)"
              , width = px 100
              , view = newWeightColumn
              }
            , { header = centeredHeader "Weigted\ngrade"
              , width = px 100
              , view = weightedGradeColumn
              }
            ]
        }


noBorders =
    { bottom = 0, left = 0, right = 0, top = 0 }


topBorder =
    Border.widthEach { noBorders | top = 2 }


bottomBorder =
    Border.widthEach { noBorders | bottom = 2 }


cell : List (Attribute msg) -> Element msg -> Element msg
cell attrs child =
    el (padding 10 :: attrs) child


header txt =
    cell [ bottomBorder, Font.bold, height fill ] <| el [ alignBottom ] <| text txt


centeredHeader txt =
    cell [ bottomBorder, Font.bold, height fill ] <| el [ centerX, alignBottom, Font.center ] <| text txt


view : Model -> Html Msg
view model =
    layout [ padding 20, Font.size 16 ] <|
        column [ spacing 20, width (px 400) ]
            [ viewTable model
            , viewFinalGrade model
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


type alias FooRecord =
    { foo : Int }


type alias BarRecord =
    { bar : Maybe Int }


type FooBar
    = Foo FooRecord
    | Bar BarRecord
