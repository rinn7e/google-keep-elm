module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }


type alias Model =
    { noteList : List NoteModel
    , modal : Modal
    , form : NoteModel
    }


model : Model
model =
    { noteList =
        [ { id = 2
          , title = "សួស្ដី"
          , desc = "នេះគឺជា កម្មវិធីដែលយកគំរូតាម Google Keep \nដោយប្រើ Elm ជាមួយនឹង Tachyons CSS"
          , pin = False
          , date = "10/02/2012 "
          , tag = [ "elm", "tachyons" ]
          }
        , { id = 1
          , title = "Hello"
          , desc = "This means to clone the look of Google Keep \nwritten in Elm with Tachyons CSS"
          , pin = False
          , date = "10/02/2012 "
          , tag = [ "elm", "tachyons" ]
          }
        ]
    , modal = NoModal
    , form = emptyNote
    }


type Modal
    = CreateModal
    | EditModal
    | NoModal


emptyNote : NoteModel
emptyNote =
    { id = 0
    , title = ""
    , desc = ""
    , pin = False
    , date = ""
    , tag = []
    }


type alias NoteModel =
    { id : Int
    , title : String
    , desc : String
    , pin : Bool
    , date : String
    , tag : List String
    }


type Msg
    = ViewNote NoteModel
    | CreateNote
    | CloseModal
    | DeleteNote
    | Input String String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ViewNote note ->
            { model | modal = EditModal, form = note }

        CreateNote ->
            let
                note =
                    { emptyNote | id = (List.length model.noteList) + 1 }
            in
                { model | modal = CreateModal, form = note }

        DeleteNote ->
            let
                newNoteList =
                    case model.modal of
                        EditModal ->
                            List.filter
                                (\note ->
                                    note.id /= model.form.id
                                )
                                model.noteList

                        _ ->
                            model.noteList
            in
                { model | modal = NoModal, noteList = newNoteList, form = emptyNote }

        CloseModal ->
            let
                newNoteList =
                    case model.modal of
                        EditModal ->
                            List.map
                                (\oldNote ->
                                    if oldNote.id == model.form.id then
                                        model.form
                                    else
                                        oldNote
                                )
                                model.noteList

                        CreateModal ->
                            if (model.form.title == "" && model.form.desc == "") then
                                model.noteList
                            else
                                model.form :: model.noteList

                        _ ->
                            model.noteList
            in
                { model | modal = NoModal, noteList = newNoteList, form = emptyNote }

        Input label value ->
            let
                modelForm =
                    model.form

                newNote =
                    case label of
                        "title" ->
                            { modelForm | title = value }

                        "desc" ->
                            { modelForm | desc = value }

                        _ ->
                            modelForm
            in
                { model | form = newNote }


view : Model -> Html Msg
view model =
    div [ class """
          w-100 w-70-l center shadow-1
          h-100 bg-near-white flex flex-column relative
          sans-serif overflow-hidden
          """ ]
        [ case model.modal of
            NoModal ->
                div [] []

            EditModal ->
                (modalContainerView (modalView model.form))

            CreateModal ->
                (modalContainerView (modalView model.form))
        , div [ class "bg-gold flex items-center white" ]
            [ mdIcon [ class "pa2" ] "menu"
            , div [ class "pa2 b" ] [ text "Note" ]
            , div [ class "flex-auto" ] []
            , mdIcon [ class "pa2" ] "search"
            ]
        , div [ class "flex-auto pa2" ]
            (model.noteList
                |> List.map noteView
            )
        , div [ class "bg-white flex items-center black-60 shadow-1" ]
            [ div [ class "pa2 flex-auto pointer", onClick CreateNote ] [ text "Take a note..." ]
            , mdIcon [ class "pa2" ] "view_list"
            , mdIcon [ class "pa2" ] "create"
            , mdIcon [ class "pa2" ] "keyboard_voice"
            , mdIcon [ class "pa2" ] "camera_alt"
            ]
        ]


noteView : NoteModel -> Html Msg
noteView note =
    div [ class "bg-white pa2 mb2 pointer ", onClick (ViewNote note) ]
        [ div [ class "b mv2" ] [ text note.title ]
        , div [ class "pre-line min-h2" ] [ text note.desc ]
        ]


modalView note =
    div [ class "bg-white w-100 h-100 flex flex-column" ]
        [ div [ class "bg-white shadow-1 flex" ]
            [ mdIcon [ class "pa2", onClick CloseModal ] "arrow_back"
            , div [ class "flex-auto" ] []
            , mdIcon [ class "pa2", onClick DeleteNote ] "archive"
            ]
        , div [ class "pa3 flex-auto flex flex-column " ]
            [ input [ class "b bn mv2 outline-0", value note.title, onInput (Input "title"), placeholder "Title" ] []
            , textarea [ class "pre-line outline-0 flex-auto sans-serif bn", onInput (Input "desc"), placeholder "Note" ]
                [ text note.desc
                ]
            ]
        , div [] []
        ]


modalContainerView view =
    div
        [ class "absolute w-100 h-100 flex justify-center items-center bg-black"
        ]
        [ view
        ]


mdIcon attr iconName =
    i ([ class "material-icons pointer" ] ++ attr)
        [ text iconName
        ]
