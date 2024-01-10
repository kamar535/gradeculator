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
import Html exposing (Html, a, br, i)
import List
import Maybe.Extra
import Round
import String
import Tuple2



-- NEW THRESHOLD HIGHER GRADE ASSIGNEMNTS


type MandatoryGrade
    = MandatoryFail
    | MandatoryPass


mandatoryGradeToGrade mg =
    case mg of
        MandatoryFail ->
            Fail

        MandatoryPass ->
            Pass


gradeToFailPass grade =
    case grade of
        Fail ->
            MandatoryFail

        Pass ->
            MandatoryPass

        _ ->
            MandatoryPass


type alias OldThresholdAssignment =
    { name : String
    , grade : MandatoryGrade
    , higherGradeMaxPoints : Int
    , higherGradePoints : Int
    }


type alias ThresholdAssignments =
    { name : String
    , code : String
    , grade4Threshold : Int
    , grade5Threshold : Int
    , assignments : List OldThresholdAssignment
    }


thresholdGrade grade4 grade5 pts max =
    let
        grade =
            if pts >= grade5 then
                5

            else if pts >= grade4 then
                4

            else
                3
    in
    { points = pts, four = grade4, five = grade5, grade = grade, max = max }


thresholdGradeV44 four five ( allPass, points, max ) =
    let
        grade =
            if allPass then
                Just <|
                    if points >= five then
                        5

                    else if points >= four then
                        4

                    else
                        3

            else
                Nothing
    in
    { points = points, four = four, five = five, grade = grade, max = max }



--thresholdGradeV2 : ThresholdAssignments -> Maybe ( Int, Int )


thresholdGradeV2 assignments =
    assignments.assignments
        |> List.map
            (\a ->
                if a.grade == MandatoryPass then
                    Just a.higherGradePoints

                else
                    Nothing
            )
        |> Maybe.Extra.combine
        |> Maybe.map List.sum
        |> Maybe.map (thresholdGrade assignments.grade4Threshold assignments.grade5Threshold)


oldFundamentalConcepts : OldThresholdAssignment
oldFundamentalConcepts =
    { name = "Fundamental concepts"
    , grade = MandatoryPass
    , higherGradeMaxPoints = 3
    , higherGradePoints = 3
    }


oldTheProcessConcept : OldThresholdAssignment
oldTheProcessConcept =
    { name = "The process"
    , grade = MandatoryPass
    , higherGradeMaxPoints = 3
    , higherGradePoints = 0
    }


oldThreadsSynchronizationDeadlock : OldThresholdAssignment
oldThreadsSynchronizationDeadlock =
    { name = "Threads, synchronization and deadlock"
    , grade = MandatoryPass
    , higherGradeMaxPoints = 4
    , higherGradePoints = 2
    }


osppA1 : ThresholdAssignments
osppA1 =
    { name = "Assignments (part 1)"
    , code = "22"
    , grade4Threshold = 6
    , grade5Threshold = 8
    , assignments =
        [ oldFundamentalConcepts
        , oldTheProcessConcept
        , oldThreadsSynchronizationDeadlock
        ]
    }



--- END new ...


type Grade
    = Fail
    | Pass
    | Three
    | Four
    | Five


gradeToString : Grade -> String
gradeToString grade =
    case grade of
        Fail ->
            "F"

        Pass ->
            "P"

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
    , assignments : Assignments
    }


type Assignments
    = SingleGrade Grade
    | AverageGrade (Array.Array AverageGradePart)
    | ThresholdGrade Int Int (Array.Array ThresholdGradePart)


viewThresholdModuleGrade : Int -> Array.Array Module -> Element Msg
viewThresholdModuleGrade moduleIndex modules =
    case moduleThresholdGradeFromModuleIndex moduleIndex modules of
        Nothing ->
            text <| "Error: Invalid module index " ++ String.fromInt moduleIndex

        Just tg ->
            column [ spacing 20 ]
                [ el [ centerX ] <|
                    text <|
                        thresholdGradeString ( tg.grade, tg.points, tg.max )
                , thresholdsV2 tg
                , row [ centerX, spacing 20 ] [ el [] <| text "Mandatory", el [] <| text "Higher grade points" ]
                ]


moduleThresholdGradeFromModuleIndex : Int -> Array.Array Module -> Maybe ThresholdResult
moduleThresholdGradeFromModuleIndex moduleIndex modules =
    Array.get moduleIndex modules
        |> Maybe.map .assignments
        |> Maybe.map thresholdGradeV4
        |> Maybe.Extra.join


thresholdGradeString : ( Maybe Int, Int, Int ) -> String
thresholdGradeString ( grade, points, max ) =
    grade
        |> Maybe.map (\g -> String.fromInt g ++ " (" ++ String.fromInt points ++ " of " ++ String.fromInt max ++ " points)")
        |> Maybe.withDefault "No grade"


thresholdGradeV3 : Assignments -> Maybe { points : Int, four : Int, five : Int, max : Int, grade : Int }
thresholdGradeV3 assignments =
    case assignments of
        SingleGrade _ ->
            Nothing

        AverageGrade _ ->
            Nothing

        ThresholdGrade four five tas ->
            tas
                |> Array.toList
                |> List.map
                    (\a ->
                        if a.grade == MandatoryPass then
                            Just ( a.higherGradePoints, a.higherGradeMaxPoints )

                        else
                            Nothing
                    )
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl (\( x, y ) ( xx, yy ) -> ( x + xx, y + yy )) ( 0, 0 ))
                |> Maybe.map (\( pts, max ) -> thresholdGrade four five pts max)



--|> Maybe.map List.sum
--|> Maybe.map (thresholdGrade four five)


type alias ThresholdResult =
    { points : Int, four : Int, five : Int, max : Int, grade : Maybe Int }


thresholdGradeV4 : Assignments -> Maybe ThresholdResult
thresholdGradeV4 assignments =
    case assignments of
        SingleGrade _ ->
            Nothing

        AverageGrade _ ->
            Nothing

        ThresholdGrade four five tas ->
            tas
                |> Array.toList
                |> List.map
                    (\a ->
                        ( a.grade == MandatoryPass, a.higherGradePoints, a.higherGradeMaxPoints )
                    )
                |> List.foldl (\( b, x, y ) ( bb, xx, yy ) -> ( b && bb, x + xx, y + yy )) ( True, 0, 0 )
                |> thresholdGradeV44 four five
                |> Just


type alias AverageGradePart =
    { name : String
    , grade : Grade
    }


type alias ThresholdGradePart =
    { name : String
    , grade : MandatoryGrade
    , higherGradeMaxPoints : Int
    , higherGradePoints : Int
    }


initThresholdGradePart : ( String, Int ) -> ThresholdGradePart
initThresholdGradePart ( name, points ) =
    { name = name
    , grade = MandatoryPass
    , higherGradeMaxPoints = points
    , higherGradePoints = 0
    }


initThresholdGradeParts : List ( String, Int ) -> Array.Array ThresholdGradePart
initThresholdGradeParts parts =
    Array.fromList <| List.map initThresholdGradePart parts


thresholdGradeOf : Int -> Int -> List ( String, Int ) -> Assignments
thresholdGradeOf four five assignments =
    ThresholdGrade four five <| initThresholdGradeParts assignments


gradeToMaybeFloat : Grade -> Maybe Float
gradeToMaybeFloat grade =
    case grade of
        Fail ->
            Nothing

        Pass ->
            Just 3

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
            text <| (String.fromInt <| round avg) ++ " (average " ++ Round.round 2 avg ++ ")"


averageModuleGradeFromIndex : Int -> Array.Array Module -> Maybe (Maybe Float)
averageModuleGradeFromIndex moduleIndex modules =
    Array.get moduleIndex modules |> Maybe.map averageModuleGrade


averageModuleGrade : Module -> Maybe Float
averageModuleGrade m =
    averageGrade m.assignments


averageGrade : Assignments -> Maybe Float
averageGrade grades =
    case grades of
        SingleGrade g ->
            gradeToMaybeFloat g

        AverageGrade gs ->
            Array.toList gs
                |> List.map .grade
                |> Maybe.Extra.traverse gradeToMaybeFloat
                |> Maybe.map List.sum
                |> Maybe.map (\tot -> tot / toFloat (Array.length gs))

        (ThresholdGrade _ _ _) as t ->
            thresholdGradeV3 t
                |> Maybe.map .grade
                |> Maybe.map toFloat



-- thresholdGradeV2


finalGrade : Array.Array { a | credits : Int, assignments : Assignments } -> Maybe ( Int, Int )
finalGrade modules =
    Array.toList modules
        |> List.map (\mod -> ( .credits mod, .assignments mod ) |> Tuple2.maybeMapSecond averageGrade)
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl (\( w, g ) ( wSum, gSum ) -> ( w + wSum, (w * round g) + gSum )) ( 0, 0 ))


defaultGrade : Grade
defaultGrade =
    Three


initAverageGradePart : String -> AverageGradePart
initAverageGradePart n =
    { name = n
    , grade = defaultGrade
    }


singleGrade : Assignments
singleGrade =
    SingleGrade defaultGrade


averageGradeOf : List String -> Assignments
averageGradeOf names =
    AverageGrade <| initAverageGradeParts names



--- OSPP Modules


a1 : Module
a1 =
    { code = "2000"
    , name = "Assignments (part 1)"
    , credits = 2
    , assignments = averageGradeOf [ "Fundamental concepts", "The process concept", "Threads, synchronization and deadlock" ]
    }


a1Threshold : Module
a1Threshold =
    { code = "2000"
    , name = "Assignments (part 1)"
    , credits = 2
    , assignments =
        thresholdGradeOf
            6
            8
            [ ( "Fundamental concepts", 3 )
            , ( "The pocess concept", 3 )
            , ( "Threads, synchronization and deadlock", 4 )
            ]
    }


e : Module
e =
    { code = "3000"
    , name = "Written exam (part 1)"
    , credits = 1
    , assignments = singleGrade
    }


a2 : Module
a2 =
    { code = "5000"
    , name = "Assignments (part 2)"
    , credits = 1
    , assignments = singleGrade
    }


pg : Module
pg =
    { code = "6000"
    , name = "Project (group)"
    , credits = 2
    , assignments = singleGrade
    }


pi : Module
pi =
    { code = "7000"
    , name = "Project (individual)"
    , credits = 5
    , assignments = singleGrade
    }


osppModules : Array.Array Module
osppModules =
    Array.fromList [ a1, a1Threshold, e, a2, pg, pi ]



--- DSP Modules
--- OSPP Modules


dspAssignments : Module
dspAssignments =
    { code = "2010"
    , name = "Assignments"
    , credits = 5
    , assignments = averageGradeOf [ "Fundamental concepts", "The process concept", "Threads, synchronization and deadlock" ]
    }


dspExam : Module
dspExam =
    { code = "1010"
    , name = "Written exam"
    , credits = 2
    , assignments = singleGrade
    }


dspProjectGroup : Module
dspProjectGroup =
    { code = "3020"
    , name = "Project (group)"
    , credits = 2
    , assignments = singleGrade
    }


dspProjectIndividual : Module
dspProjectIndividual =
    { code = "3010"
    , name = "Project (individual)"
    , credits = 5
    , assignments = singleGrade
    }


dspModules =
    Array.fromList [ dspExam, dspAssignments, dspProjectIndividual, dspProjectGroup ]



---


initAverageGradeParts : List String -> Array.Array AverageGradePart
initAverageGradeParts names =
    Array.fromList <| List.map initAverageGradePart names


type alias Model =
    Array.Array Module


type ButtonPosition
    = First
    | Middle
    | Last


type Msg
    = SetModuleGrade Int Grade
    | SetPartGrade Int Int Grade
    | SetHigherGradePoints Int Int Int


viewFinalGrade : Array.Array { a | credits : Int, assignments : Assignments } -> Element Msg
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


thresholds : { points : Int, four : Int, five : Int, max : Int, grade : Int } -> Element Msg
thresholds rec =
    let
        a =
            rec.four * 2

        b =
            (rec.five - rec.four) * 2

        c =
            (rec.max - rec.four) * 2

        d =
            rec.points * 2

        ee =
            (rec.max - rec.points) * 2

        bar =
            row [ width (px 300), height (px 15) ]
                [ el [ width (fillPortion 1) ] none
                , el [ height fill, width (fillPortion d), Background.color color.blue ] none
                , el [ width (fillPortion ee) ] none
                , el [ width (fillPortion 1) ] none
                ]
    in
    column []
        [ row [ width (px 300), behindContent bar ]
            [ el [ width (fillPortion 1) ] none
            , el [ width (fillPortion a), Border.color color.black, Border.width 1 ] <| text "3"
            , el [ width (fillPortion b), Border.color color.black, Border.width 1 ] <| text "4"
            , el [ width (fillPortion c), Border.color color.black, Border.width 1 ] <| text "5"
            , el [ width (fillPortion 1) ] none
            ]
        , row [ width (px 300) ]
            [ el [ width (fillPortion 2) ] <| text "0"
            , el [ width (fillPortion <| (2 * rec.four) - 2) ] <| none
            , el [ width (fillPortion 2) ] <| text <| String.fromInt rec.four
            , el [ width (fillPortion <| (2 * (rec.five - rec.four)) - 2) ] <| none
            , el [ width (fillPortion 2) ] <| text <| String.fromInt rec.five
            , el [ width (fillPortion <| (2 * (rec.max - rec.four)) - 2) ] <| none
            , el [ width (fillPortion 2) ] <| text <| String.fromInt rec.max
            ]
        ]


pills : Int -> Int -> Int -> Element Msg
pills start n nColored =
    row [ spacing 5, Font.size 9 ] <|
        (List.range start (start + nColored - 1)
            |> List.map (\i -> el [ centerX, centerY, width (px 16), height (px 16), Border.rounded 4, Border.color color.gray, Border.width 1, Background.color color.blue ] <| el [ centerX, centerY ] <| text <| String.fromInt i)
        )
            ++ (List.range (start + nColored) (start + nColored + (n - nColored) - 1)
                    |> List.map (\i -> el [ width (px 16), height (px 16), Border.rounded 4, Border.color color.gray, Border.width 1, Background.color color.white ] <| el [ centerX, centerY ] <| text <| String.fromInt i)
               )


thresholdsV2 : ThresholdResult -> Element Msg
thresholdsV2 rec =
    let
        pThree =
            rec.four - 1

        pFour =
            rec.five - rec.four

        pFive =
            rec.max - rec.five + 1

        threePoints =
            if rec.points < rec.four then
                rec.points

            else
                pThree

        fourPoints =
            if rec.points >= rec.four then
                Basics.min pFour (rec.points - threePoints)

            else
                0

        fivePoints =
            if rec.points >= rec.five then
                Basics.min pFive (rec.points - threePoints - fourPoints)

            else
                0

        labelAttribute label =
            centerX
                :: (rec.grade
                        |> Maybe.map
                            (\g ->
                                if label == g then
                                    [ Font.bold ]

                                else
                                    []
                            )
                        |> Maybe.withDefault []
                   )
    in
    row [ spacing 16 ]
        [ column [ spacing 10 ]
            [ el [ width (fillPortion pThree) ] (pills 1 pThree threePoints)
            , el (labelAttribute 3) <| text "Three"
            ]
        , column [ spacing 10 ]
            [ el [ width (fillPortion pFour) ] (pills rec.four pFour fourPoints)
            , el (labelAttribute 4) <| text "Four"
            ]
        , column [ spacing 10 ]
            [ el [ width (fillPortion pFive) ] (pills rec.five pFive fivePoints)
            , el (labelAttribute 5) <| text "Five"
            ]
        ]


htmlTable : Model -> Html Msg
htmlTable model =
    Html.table []
        [ Html.tr []
            [ Html.td [] [ Html.text "one" ]
            , Html.td [] [ Html.text "two" ]
            , Html.td [] [ Html.text "three" ]
            ]
        , Html.tr []
            [ Html.td [] [ Html.text "one" ]
            , Html.td [] [ layoutWith { options = [ noStaticStyleSheet ] } [] <| row [] [ el [] <| gradeButtons (ModuleIndex 2) model ] ]
            , Html.td [] [ Html.text "three" ]
            ]
        ]


view : Model -> Html Msg
view model =
    layout [ padding 20 ] <|
        column [ spacing 20, Font.size 12 ]
            [ modulesTable model
            , viewFinalGrade model

            -- , thresholds 6 8 10 4
            -- , thresholdsV2 6 8 10 7 4
            , html <| htmlTable model
            ]


type GradeIndex
    = ModuleIndex Int
    | ModuleAndPartIndex Int Int


getSingleGrade : Assignments -> Maybe Grade
getSingleGrade assignment =
    case assignment of
        SingleGrade g ->
            Just g

        AverageGrade _ ->
            Nothing

        ThresholdGrade _ _ _ ->
            Nothing


getAverageGrades : Assignments -> Maybe (Array.Array AverageGradePart)
getAverageGrades assignments =
    case assignments of
        AverageGrade gs ->
            Just gs

        SingleGrade _ ->
            Nothing

        ThresholdGrade _ _ _ ->
            Nothing


getThresholdFailPass : Assignments -> Maybe (Array.Array ThresholdGradePart)
getThresholdFailPass assignments =
    case assignments of
        AverageGrade _ ->
            Nothing

        SingleGrade _ ->
            Nothing

        ThresholdGrade _ _ tas ->
            Just tas


higherGradePointsButtons : Int -> Int -> Int -> Maybe Int -> Element Msg
higherGradePointsButtons moduleIndex partIndex maxPoints selected =
    let
        option : Int -> ButtonPosition -> Input.Option Int Msg
        option pts position =
            Input.optionWith pts (button position color.blue <| String.fromInt pts)

        msg =
            SetHigherGradePoints moduleIndex partIndex
    in
    Input.radioRow
        [ Border.rounded 6
        , width (px 90)
        ]
        { onChange = msg
        , selected = selected
        , label =
            Input.labelHidden "Select higher grade points"
        , options =
            option 0 First
                :: List.map (\x -> option x Middle) (List.range 1 (maxPoints - 1))
                ++ [ option maxPoints Last ]
        }


failPassButtons : GradeIndex -> Array.Array Module -> Element Msg
failPassButtons gradeIndex modules =
    let
        option : Grade -> ButtonPosition -> Input.Option Grade msg
        option g position =
            Input.optionWith g (gradeButton g position (gradeToString g))

        grade : Maybe Grade
        grade =
            case gradeIndex of
                ModuleIndex _ ->
                    Nothing

                ModuleAndPartIndex m p ->
                    Array.get m modules
                        |> Maybe.map .assignments
                        |> Maybe.andThen getThresholdFailPass
                        |> Maybe.andThen (Array.get p)
                        |> Maybe.map .grade
                        |> Maybe.map mandatoryGradeToGrade

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
            , option Pass Last
            ]
        }


button : ButtonPosition -> Color -> String -> Input.OptionState -> Element Msg
button position c label state =
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
                    Maybe.map .assignments (Array.get i modules)
                        |> Maybe.andThen getSingleGrade

                ModuleAndPartIndex m p ->
                    Array.get m modules
                        |> Maybe.map .assignments
                        |> Maybe.andThen getAverageGrades
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
    | AveragePart { name : String, moduleIndex : Int, partIndex : Int }
    | ThresholdHeader { code : String, credits : Int, name : String, moduleIndex : Int }
    | ThresholdRow { name : String, moduleIndex : Int, partIndex : Int, maxPoints : Int }
    | Sum


getNameAndGrade : Int -> Array.Array AverageGradePart -> ( String, Grade )
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
    case m.assignments of
        SingleGrade _ ->
            [ Single { code = m.code, credits = m.credits, name = m.name, moduleIndex = moduleIndex } ]

        AverageGrade grades ->
            Header { code = m.code, credits = m.credits, name = m.name, moduleIndex = moduleIndex }
                :: Array.Extra.indexedMapToList
                    (\i part ->
                        AveragePart { name = part.name, moduleIndex = moduleIndex, partIndex = i }
                    )
                    grades

        ThresholdGrade four five assignments ->
            ThresholdHeader { code = m.code, credits = m.credits, name = m.name, moduleIndex = moduleIndex }
                :: Array.Extra.indexedMapToList
                    (\i assignment ->
                        ThresholdRow { name = assignment.name, moduleIndex = moduleIndex, partIndex = i, maxPoints = assignment.higherGradeMaxPoints }
                    )
                    assignments


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

        ThresholdHeader record ->
            .code record |> text |> tableCell [ centerX ]

        AveragePart _ ->
            none

        ThresholdRow _ ->
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

        ThresholdHeader record ->
            .credits record |> String.fromInt |> text |> tableCell [ centerX ]

        AveragePart _ ->
            none

        ThresholdRow _ ->
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


nameCell name =
    row []
        [ el [ width (px 20) ] none
        , paragraph [ Font.alignLeft ] [ name |> text ]
        ]
        |> tableCell []


nameColumn : Row -> Element msg
nameColumn r =
    case r of
        Single record ->
            .name record |> text |> tableCell []

        Header record ->
            paragraph [ Font.alignLeft ] [ .name record |> text ] |> tableCell []

        ThresholdHeader record ->
            paragraph [ Font.alignLeft ] [ .name record |> text ] |> tableCell []

        AveragePart record ->
            nameCell record.name

        ThresholdRow record ->
            nameCell record.name

        Sum ->
            sumCell [] none


gradeColumn : Array.Array Module -> Row -> Element Msg
gradeColumn modules r =
    case r of
        Single mod ->
            tableCell [ centerX ] <| gradeButtons (ModuleIndex mod.moduleIndex) modules

        Header h ->
            tableCell [ centerX ] <| viewAverageModuleGrade h.moduleIndex modules

        ThresholdHeader h ->
            tableCell [ centerX ] <| viewThresholdModuleGrade h.moduleIndex modules

        AveragePart part ->
            tableCell [ centerX ] <| gradeButtons (ModuleAndPartIndex part.moduleIndex part.partIndex) modules

        ThresholdRow assignment ->
            let
                m =
                    assignment.moduleIndex

                p =
                    assignment.partIndex

                selected : Maybe Int
                selected =
                    Array.get m modules
                        |> Maybe.map .assignments
                        |> Maybe.andThen getThresholdFailPass
                        |> Maybe.andThen (Array.get p)
                        |> Maybe.map .higherGradePoints
            in
            tableCell [ centerX ] <|
                row [ centerX, spacing 20 ]
                    [ el [ width (px 60), centerX ] <| failPassButtons (ModuleAndPartIndex assignment.moduleIndex assignment.partIndex) modules
                    , higherGradePointsButtons assignment.moduleIndex assignment.partIndex assignment.maxPoints selected
                    ]

        Sum ->
            sumCell [ Font.bold, alignRight ] <| text "Sum"


weightedColumn : Array.Array Module -> Row -> Element Msg
weightedColumn modules w =
    case w of
        Single s ->
            tableCell [ centerX ] <| viewModuleAverageWeightedGrade s.moduleIndex modules

        Header h ->
            tableCell [ centerX ] <| viewModuleAverageWeightedGrade h.moduleIndex modules

        ThresholdHeader h ->
            tableCell [ centerX ] <| viewModuleAverageWeightedGrade h.moduleIndex modules

        AveragePart _ ->
            none

        ThresholdRow _ ->
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
                (\mod -> { mod | assignments = SingleGrade grade })
                model

        SetPartGrade moduleIndex partIndex grade ->
            Array.Extra.update moduleIndex
                (\mod ->
                    case mod.assignments of
                        SingleGrade _ ->
                            mod

                        AverageGrade grades ->
                            { mod
                                | assignments =
                                    AverageGrade
                                        (Array.Extra.update
                                            partIndex
                                            (\part -> { part | grade = grade })
                                            grades
                                        )
                            }

                        ThresholdGrade four five assignments ->
                            { mod
                                | assignments =
                                    ThresholdGrade four
                                        five
                                        (Array.Extra.update
                                            partIndex
                                            (\part -> { part | grade = gradeToFailPass grade })
                                            assignments
                                        )
                            }
                )
                model

        SetHigherGradePoints moduleIndex assignmentIndex pts ->
            setHigherGradePoints moduleIndex
                assignmentIndex
                pts
                model



{- Array.Extra.update moduleIndex
   (\m ->
       case m.assignments of
           SingleGrade _ -> m
           AverageGrade _ -> m
           ThresholdGrade four five as ->
               {m | assignments = ThresholdGrade four five (Array.Extra.update assignmentIndex (\a -> {a | higherGradePoints = })))})
-}


setHigherGradePoints : Int -> Int -> Int -> Model -> Model
setHigherGradePoints moduleIndex assignmentIndex pts model =
    Array.Extra.update moduleIndex
        (\mod ->
            case mod.assignments of
                SingleGrade _ ->
                    mod

                AverageGrade _ ->
                    mod

                ThresholdGrade four five assignments ->
                    { mod
                        | assignments =
                            ThresholdGrade four
                                five
                                (Array.Extra.update
                                    assignmentIndex
                                    (\part -> { part | higherGradePoints = pts })
                                    assignments
                                )
                    }
        )
        model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = osppModules -- dspModules -- osppModules
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
