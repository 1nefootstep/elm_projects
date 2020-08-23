module TranslationSite exposing (..)
import Browser
import Html exposing (Attribute, Html, button, div, input, text, textarea)
import Html.Attributes exposing (cols, rows, size, style, value)
import Html.Events exposing (onClick, onInput)
import Input.Number
import List exposing (foldl)
import Regex

-- MODEL
type alias Model =
    { chapterNumber : Int
    , title : String
    , linkPrefix : String
    , credits : String
    , input : String
    , output : List String
    , minWordCountPerPage : Int
    }

type Msg
    = SaveWordCountPerPage (Maybe Int)
    | SaveInput String
    | SaveTitle String
    | SaveLinkPrefix String
    | SaveCredits String
    | SaveChapterNumber String
    | GenerateOutput

-- VIEW
view : Model -> Html Msg
view model =
    div [style "text-align" "center"]
        [ viewWordCount model
        , viewChapterNumber
        , viewTitle
        , viewLinkPrefix model
        , viewCredits model
        , viewInput
        , viewSubmitButton
        , viewOutputs model
        ]

viewWordCount: Model -> Html Msg
viewWordCount model =
    div []
        [ text "Min word count per page: "
        , Input.Number.input
            { onInput = SaveWordCountPerPage
            , maxLength = Nothing
            , maxValue = Nothing
            , minValue = Just 100
            , hasFocus = Nothing
            }
            []
            (Just model.minWordCountPerPage)
        ]

viewTitle : Html Msg
viewTitle =
    div []
        [ text "Title: "
        , input
            [ onInput SaveTitle
            , size 50
            ]
            []
        ]

viewLinkPrefix : Model -> Html Msg
viewLinkPrefix model =
    div []
        [ text "Link prefix: "
        , input
            [ onInput SaveLinkPrefix
            , size 50
            , value model.linkPrefix
            ]
            []
        ]

viewChapterNumber : Html Msg
viewChapterNumber =
    div []
        [ text "Chapter Num: "
        , input
            [ onInput SaveChapterNumber
            ]
            []
        ]

viewCredits: Model -> Html Msg
viewCredits model =
    div []
        [ text "Credits: "
        , textarea
            [ onInput SaveCredits
            , rows 10
            , cols 50
            , value model.credits
            ]
            []
        ]

viewInput: Html Msg
viewInput =
    div []
        [ text "Main text: "
        , textarea
            [ onInput SaveInput
            , rows 30
            , cols 100
            ]
            []
        ]

viewOutputs : Model -> Html Msg
viewOutputs model =
    div []
        ( List.indexedMap
            viewOutput
            model.output
        )


viewOutput : Int -> String -> Html Msg
viewOutput int output =
    div []
        [ text ("page number:" ++ (String.fromInt (int + 1)))
        , textarea
            [ rows 10
            , cols 50
            ]
            [ text output ]
        ]

viewSubmitButton : Html Msg
viewSubmitButton =
    div []
        [ button
            [ onClick GenerateOutput ]
            [ text "Generate output" ]
        ]

-- UPDATE

update : Msg -> Model -> Model
update msg model =
    case msg of
        SaveWordCountPerPage maybeInt ->
            case maybeInt of
                Just i ->
                    updateWordCount i model
                Nothing ->
                    model

        SaveTitle string ->
            updateTitle string model

        SaveLinkPrefix string ->
            updateLinkPrefix string model

        SaveChapterNumber string ->
            updateChapterNumber string model

        SaveCredits string ->
            updateCredits string model

        SaveInput string ->
            updateInput string model

        GenerateOutput ->
            genOutput model

updateTitle : String -> Model -> Model
updateTitle string model =
    { model | title = string }

updateLinkPrefix : String -> Model -> Model
updateLinkPrefix string model =
    { model | linkPrefix = string }

updateChapterNumber : String -> Model -> Model
updateChapterNumber string model =
    { model | chapterNumber = Maybe.withDefault 0 (String.toInt string) }

updateWordCount : Int -> Model -> Model
updateWordCount i model =
    { model | minWordCountPerPage = i}

updateCredits : String -> Model -> Model
updateCredits string model =
    { model | credits = string }

updateInput : String -> Model -> Model
updateInput string model =
    { model | input = string }

genOutput : Model -> Model
genOutput model =
    { model
    | output =
        breakIntoParagraphs model.input
                |> withWordCount
                |> (splitIntoPages model.minWordCountPerPage)
                |> List.reverse
                |> dropEarlyBrs
    }
    |> addLinksToPages

breakIntoParagraphs : String -> List String
breakIntoParagraphs string =
    let
        maybeRegex = Regex.fromString "[\\r\\n|\\r|\\n]+"
    in
    case maybeRegex of
        Just regex ->
            Regex.split regex string
        Nothing ->
            ["Regex failed."]

withWordCount : List String -> List ( Int, String )
withWordCount sentences =
    let
        wordCountSentences : List Int
        wordCountSentences =
            List.map
                (\sentence ->
                    String.words sentence
                        |> List.length
                )
                sentences
    in
    List.map2
        Tuple.pair
        wordCountSentences
        sentences

type alias SplitPagesHelper =
    { pages : List String
    , current : ( Int, String )
    , leftovers: List ( Int, String )
    }

splitIntoPages : Int -> List ( Int , String ) -> List String
splitIntoPages minWordCount sWithWc =
    let
        extractLeftovers : List ( Int, String ) -> ( Maybe ( Int, String ), List ( Int, String ) )
        extractLeftovers s =
            let
                maybeHead = List.head s
                maybeTail = List.tail s
            in
            case maybeTail of
                Just tail ->
                    ( maybeHead, tail )
                Nothing ->
                    ( maybeHead, [] )

        createPage : SplitPagesHelper -> SplitPagesHelper
        createPage a =
            if (Tuple.first a.current) > minWordCount then
                { a
                | pages = (Tuple.second a.current) :: a.pages
                , current = ( 0, "" )
                }
            else
                a

        joinParagraphs : String -> String -> String
        joinParagraphs p1 p2 =
            let
                lineGap = "<br /><br />"
            in
            p1 ++ lineGap ++ p2

        helper : SplitPagesHelper -> List String
        helper a =
            let
                extractedLeftovers = extractLeftovers a.leftovers
            in
            case (Tuple.first extractedLeftovers) of
                Nothing ->
                    if (Tuple.first a.current) > 0 then
                        (Tuple.second a.current) :: a.pages
                    else
                        a.pages
                Just s ->
                    createPage
                        { a
                        | current =
                            ( Tuple.first a.current + (Tuple.first s)
                            , joinParagraphs
                                (Tuple.second a.current)
                                (Tuple.second s)
                            )
                        , leftovers = Tuple.second extractedLeftovers
                        }
                        |> helper
    in
    helper { pages = [], current = ( 0, "" ), leftovers = sWithWc }

dropEarlyBrs : List String -> List String
dropEarlyBrs pages =
    List.indexedMap
        (\index page ->
            if index == 0 then
                page
            else
                String.dropLeft 12 page
        )
        pages

addLinksToPages : Model -> Model
addLinksToPages model =
    let
        pages = model.output

        chapterLinks =
            "<center><span chapter=\""
            ++ (String.fromInt (model.chapterNumber - 15))
            ++ "\" class=\"orPrevNext\">Loading......</span></center>"
    in
    { model
    | output =
        List.indexedMap
            (addLinksToPage model.title model.linkPrefix (List.length pages) chapterLinks model.credits)
            pages
    }


addLinksToPage : String -> String -> Int -> String -> String -> Int -> String -> String
addLinksToPage title linkPrefix lastPageNumber chapterLinks credits pgNumFromZero page =
    let
        currPageNum = pgNumFromZero + 1

        preLink = linkPrefix ++ spaceToDash title ++ "-p"
        postLink = ".html\">"

        addSingleLink : Int -> String -> String
        addSingleLink pgNum text =
            "<a href=\""
            ++ preLink
            ++ (String.fromInt pgNum)
            ++ postLink
            ++ text
            ++ "</a>"

        addPrevPage : String -> String
        addPrevPage links =
            if currPageNum == 1 then
                links
            else
                addSingleLink
                    (currPageNum - 1)
                    "Prev Page"
                ++ " | "
                ++ links

        addNextPage : String -> String
        addNextPage links =
            if currPageNum == lastPageNumber then
                links
            else
                links
                ++ " | "
                ++ addSingleLink
                    (currPageNum + 1)
                    "Next Page"

        addCenter : String -> String
        addCenter links =
            "<center>"
            ++ links
            ++ "</center>"

        defaultLinks =
            List.range 1 lastPageNumber
                |> List.map
                    (\pgNum ->
                        if (pgNum == currPageNum) then
                            (String.fromInt pgNum)
                        else
                            addSingleLink pgNum (String.fromInt pgNum)
                    )
                |> foldl
                    (\link links
                        -> links
                        ++ " | "
                        ++ link
                    )
                    ""
                |> String.dropLeft 3
                |> addPrevPage
                |> addNextPage
                |> addCenter

        top : String
        top =
            if currPageNum == 1 then
                chapterLinks
                ++ "<hr /><h4>"
                ++ credits
                ++ "</h4>"
            else
                defaultLinks
                ++ "<hr />"

        btm : String
        btm =
            if currPageNum == lastPageNumber then
                "<hr />"
                ++ chapterLinks
            else
                "<hr />"
                ++ defaultLinks
    in
    top
    ++ page
    ++ btm


spaceToDash : String -> String
spaceToDash string =
    let
        regex =
            Regex.fromString "[\\s]+(?!\\s)"
            |> Maybe.withDefault Regex.never
    in
    Regex.replace regex (\match -> "-") string


-- MAIN
initModel : Model
initModel =
    { chapterNumber = 0
    , title = ""
    , linkPrefix = "https://www.yamitranslations.com/p/"
    , credits = "Translator: yAmi"
    , input = ""
    , output = []
    , minWordCountPerPage = 1500
    }

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        }
