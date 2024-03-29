module App

open Browser.Dom
open Fable.Core.JsInterop
open Framework
open Preact
open SyncServer

let handleEvent (state: 'model) dispatch (e: Event) =
    let mutable newState = state

    match e with
    | :? MessagesRequested<'msg> as pe ->
        let (MessagesRequested (username, server, pass, next)) = pe

        async {
            let! response = MessageUploder.loadAllPayloads username server pass
            let m = next (Ok response)
            dispatch m
        }
        |> Async.StartImmediate
    | :? NavigationChanged as pe ->
        let (NavigationChanged page) = pe
        document.location.search <- $"?page={page}"
    | :? ModelChanged<'model> as pe ->
        let (ModelChanged ns) = pe
        newState <- ns
    | :? NewMessageCreated<'msg> as ef ->
        let (NewMessageCreated (username, host, pass, payload, f)) = ef

        async {
            do! MessageUploder.upload username host pass payload
            let m = f (Ok())
            dispatch m
        }
        |> Async.StartImmediate
    | _ -> ()

    newState

let NavigationViewComponent activeIndex dispatch =
    let tabs =
        [ "Home", NavigationComponent.HomeClicked
          "List", NavigationComponent.ListClicked
          "Picture", NavigationComponent.PictureClicked ]

    let item i title msg =
        li [ "onclick" ==> fun _ -> dispatch msg
             "class"
             ==> (if i = activeIndex then
                      "is-active"
                  else
                      "") ] [
            a [] [ str title ]
        ]

    div [ "class" ==> "tabs is-boxed" ] [
        ul [] (tabs |> List.mapi (fun i (t, m) -> item i t m))
    ]

let PictureViewComponent (props: _) =
    let (model, dispatch) =
        ElmHooks.useElm handleEvent PictureComponent.init PictureComponent.update

    div [ "class" ==> "form" ] [
        NavigationViewComponent 2 (PictureComponent.NavigationClicked >> dispatch)
        label [ "class" ==> "label" ] [
            str "Messages"
        ]
        input [ "class" ==> "input"
                "type" ==> "file"
                "accept" ==> "image/*"
                "onInput"
                ==> fun e -> dispatch (PictureComponent.PathChanged e?target?value) ] []
        button [ "class" ==> "button" ] [
            str "Upload"
        ]
        label [ "class" ==> "label" ] [
            str "Auth"
        ]
        input [ "class" ==> "input"
                "placeholder" ==> "Username"
                "value" ==> model.username
                "onInput"
                ==> fun e -> dispatch (PictureComponent.UsernameChanged e?target?value) ] []
        input [ "class" ==> "input"
                "placeholder" ==> "Password"
                "type" ==> "password"
                "value" ==> model.password
                "onInput"
                ==> fun e -> dispatch (PictureComponent.PasswordChanged e?target?value) ] []
    ]

let ListViewComponent (props: _) =
    let (model, dispatch) =
        ElmHooks.useElm handleEvent ListComponent.init ListComponent.update

    let listView =
        model.items
        |> Array.mapi (fun i item ->
            div [ "class" ==> "card" ] [
                div [ "class" ==> "card-content" ] [
                    div [ "class" ==> "media" ] [
                        div [ "class" ==> "media-content" ] [
                            div [ "class" ==> "title is-4" ] [
                                str item.url
                            ]
                            div [ "class" ==> "subtitle is-6" ] [
                                str item.title
                            ]
                        ]
                    ]
                ]
                footer [ "class" ==> "card-footer" ] [
                    a [ "class" ==> "card-footer-item"
                        "onclick"
                        ==> fun _ -> dispatch (ListComponent.DeleteClicked i) ] [
                        str "Delete"
                    ]
                ]
            ])
        |> div [ "class" ==> "list" ]

    div [ "class" ==> "form" ] [
        NavigationViewComponent 1 (ListComponent.NavigationClicked >> dispatch)
        label [ "class" ==> "label" ] [
            str "Auth"
        ]
        input [ "class" ==> "input"
                "placeholder" ==> "Username"
                "value" ==> model.username
                "onInput"
                ==> fun e -> dispatch (ListComponent.UsernameChanged e?target?value) ] []
        input [ "class" ==> "input"
                "placeholder" ==> "Password"
                "type" ==> "password"
                "value" ==> model.pass
                "onInput"
                ==> fun e -> dispatch (ListComponent.PasswordChanged e?target?value) ] []
        label [ "class" ==> "label" ] [
            str "Messages"
        ]
        listView
        button [ "class" ==> "button"
                 "disabled" ==> model.buttonDisabled
                 "onclick"
                 ==> fun _ -> dispatch ListComponent.LoadMessagesClicked ] [
            str "Load items"
        ]
    ]

let HomeViewComponent (props: _) =
    let (model, dispatch) =
        ElmHooks.useElm handleEvent HomeComponent.init HomeComponent.update

    div [ "class" ==> "form" ] [
        NavigationViewComponent 0 (HomeComponent.NavigationClicked >> dispatch)
        label [ "class" ==> "label" ] [
            str "Link config"
        ]
        input [ "class" ==> "input"
                "placeholder" ==> "URL"
                "value" ==> model.url
                "disabled" ==> model.inputDisabled
                "onInput"
                ==> fun e -> dispatch (HomeComponent.UrlChanged e?target?value) ] []
        textarea [ "class" ==> "textarea"
                   "placeholder" ==> "Title"
                   "value" ==> model.title
                   "disabled" ==> model.inputDisabled
                   "onInput"
                   ==> fun e -> dispatch (HomeComponent.TitleChanged e?target?value) ] []
        div [ "class" ==> "field" ] [
            label [ "class" ==> "label" ] [
                str "Link type"
            ]
            div [ "class" ==> "control" ] [
                div [ "class" ==> "select" ] [
                    select
                        [ "name" ==> "type"
                          "value" ==> model.linkType
                          "onChange"
                          ==> fun e -> dispatch (HomeComponent.LinkTypeChanged e?target?value)
                          "disabled" ==> model.inputDisabled ]
                        (Array.mapi (fun i x -> option [ "value" ==> i ] [ str x ]) model.linkTypes)
                ]
            ]
        ]
        button [ "class" ==> "button"
                 "disabled" ==> model.buttonDisabled
                 "onclick" ==> fun _ -> dispatch HomeComponent.Add ] [
            str "Add"
        ]
        label [ "class" ==> "label" ] [
            str "Auth"
        ]
        input [ "class" ==> "input"
                "placeholder" ==> "Username"
                "value" ==> model.username
                "onInput"
                ==> fun e -> dispatch (HomeComponent.UsernameChanged e?target?value) ] []
        input [ "class" ==> "input"
                "placeholder" ==> "Password"
                "type" ==> "password"
                "value" ==> model.serverPass
                "onInput"
                ==> fun e -> dispatch (HomeComponent.PasswordChanged e?target?value) ] []
    ]

match document.location.search with
| "?page=list" -> render (comp (ListViewComponent, (), [])) (document.getElementById "root")
| "?page=pic" -> render (comp (PictureViewComponent, (), [])) (document.getElementById "root")
| _ -> render (comp (HomeViewComponent, (), [])) (document.getElementById "root")
