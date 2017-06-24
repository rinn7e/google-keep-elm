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
    , sidebarStatus : Bool
    }


model : Model
model =
    { noteList =
        [ { id = 5
          , title = "សួស្ដី"
          , desc = "នេះគឺជា កម្មវិធីដែលយកគំរូតាម Google Keep \nដោយប្រើ Elm ជាមួយនឹង Tachyons CSS"
          , pin = False
          , date = "10/02/2012 "
          , tag = [ "elm", "tachyons" ]
          }
        , { id = 4
          , title = "Hello"
          , desc = "This means to clone the look of Google Keep \nwritten in Elm with Tachyons CSS\ntest\ntest\ntest\ntest"
          , pin = False
          , date = "10/02/2012 "
          , tag = [ "elm", "tachyons" ]
          }
        , { id = 3
          , title = "Hello"
          , desc = "This means to clone the look of Google Keep \nwritten in Elm with Tachyons CSS\ntest\ntest\ntest\ntest"
          , pin = False
          , date = "10/02/2012 "
          , tag = [ "elm", "tachyons" ]
          }
        , { id = 2
          , title = "Hello"
          , desc = "This means to clone the look of Google Keep \nwritten in Elm with Tachyons CSS\ntest\ntest\ntest\ntest"
          , pin = False
          , date = "10/02/2012 "
          , tag = [ "elm", "tachyons" ]
          }
        , { id = 1
          , title = "Hello"
          , desc = "This means to clone the look of Google Keep \nwritten in Elm with Tachyons CSS\ntest\ntest\ntest\ntest"
          , pin = False
          , date = "10/02/2012 "
          , tag = [ "elm", "tachyons" ]
          }
        ]
    , modal = NoModal
    , form = emptyNote
    , sidebarStatus = False
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
    | OpenSidebar
    | CloseSidebar
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

        OpenSidebar ->
            { model | sidebarStatus = True }

        CloseSidebar ->
            { model | sidebarStatus = False }


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
        , case model.sidebarStatus of
            True ->
                (sidebarContainerView sidebarView)

            False ->
                div [] []
        , div [ class "bg-gold flex items-center white no-shrink" ]
            [ mdIcon [ class "pa3", onClick OpenSidebar ] "menu"
            , div [ class "pa3 b" ] [ text "Note" ]
            , div [ class "flex-auto" ] []
            , mdIcon [ class "pa3" ] "search"
            ]
        , div [ class "flex-auto pa2 overflow-auto" ]
            (model.noteList
                |> List.map noteView
            )
        , div [ class "bg-white flex items-center black-60 shadow-1 no-shrink" ]
            [ div [ class "pa3 flex-auto pointer", onClick CreateNote ] [ text "Take a note..." ]
            , mdIcon [ class "pa3" ] "view_list"
            , mdIcon [ class "pa3" ] "create"
            , mdIcon [ class "pa3" ] "keyboard_voice"
            , mdIcon [ class "pa3" ] "camera_alt"
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
            [ mdIcon [ class "pa3", onClick CloseModal ] "arrow_back"
            , div [ class "flex-auto" ] []
            , mdIcon [ class "pa3", onClick DeleteNote ] "archive"
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


sidebarContainerView view =
    div []
        [ div [ onClick CloseSidebar, class "absolute w-100 h-100 bg-black-transpa" ] []
        , view
        ]


sidebarView =
    div [ class "absolute w5 h-100 bg-white shadow-1" ]
        [ div
            [ class " bg-white mb2 pa2 bg-center cover"
            , style [ ( "background-image", "url('assets/bg3.jpg')" ) ]
            ]
            [ img [ class "br-100 w3 h3 mb3 bg-green", src "assets/profile.jpg" ] []
            , div [ class "mb1 white" ] [ text "Created by rinn7e" ]
            , div [ class "f6 white" ] [ text "moremi.va@gmail.com" ]
            ]
        , div [ class "flex items-center pv2 ph3 hover-bg-near-white pointer" ]
            [ mdIcon [ class "mr3" ] "archive"
            , div [] [ text "Note" ]
            ]
        , div [ class "flex items-center pv2 ph3 hover-bg-near-white pointer" ]
            [ mdIcon [ class "mr3" ] "archive"
            , div [] [ text "About" ]
            ]
        ]


mdIcon attr iconName =
    i ([ class "noselect material-icons pointer" ] ++ attr)
        [ text iconName
        ]
