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
        let (MessagesRequested (server, pass, next)) = pe

        async {
            let! response = MessageUploder.loadAllPayloads server pass
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
        let (NewMessageCreated (host, pass, payload, f)) = ef

        async {
            do! MessageUploder.upload host pass payload
            let m = f (Ok())
            dispatch m
        }
        |> Async.StartImmediate
    | _ -> ()

    newState

let ListViewComponent (props: _) =
    let (model, dispatch) =
        ElmHooks.useElm handleEvent ListComponent.init ListComponent.update

    let listView =
        model.items
        |> Array.map (fun item ->
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
            ])
        |> div []

    div [ "class" ==> "form" ] [
        button [ "class" ==> "button"
                 "onclick"
                 ==> fun _ -> dispatch ListComponent.HomeClicked ] [
            str "Open home"
        ]
        label [ "class" ==> "label" ] [
            str "Messages"
        ]
        input [ "class" ==> "input"
                "placeholder" ==> "Password"
                "value" ==> model.pass
                "onInput"
                ==> fun e -> dispatch (ListComponent.PasswordChanged e?target?value) ] []
        button [ "class" ==> "button"
                 "onclick"
                 ==> fun _ -> dispatch ListComponent.LoadMessagesClicked ] [
            str "Load items"
        ]
        listView
    ]

let HomeViewComponent (props: _) =
    let (vm, dispatch) =
        ElmHooks.useElm handleEvent HomeComponent.init (Preferences.decorate HomeComponent.update)

    div [ "class" ==> "form" ] [
        button [ "class" ==> "button"
                 "onclick"
                 ==> fun _ -> dispatch HomeComponent.ListClicked ] [
            str "Open list"
        ]
        label [ "class" ==> "label" ] [
            str "Link config"
        ]
        input [ "class" ==> "input"
                "placeholder" ==> "URL"
                "value" ==> vm.url
                "disabled" ==> vm.inputDisabled
                "onInput"
                ==> fun e -> dispatch (HomeComponent.UrlChanged e?target?value) ] []
        textarea [ "class" ==> "textarea"
                   "placeholder" ==> "Title"
                   "value" ==> vm.title
                   "disabled" ==> vm.inputDisabled
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
                          "value" ==> vm.linkType
                          "onChange"
                          ==> fun e -> dispatch (HomeComponent.LinkTypeChanged e?target?value)
                          "disabled" ==> vm.inputDisabled ]
                        (Array.mapi (fun i x -> option [ "value" ==> i ] [ str x ]) vm.linkTypes)
                ]
            ]
        ]
        button [ "class" ==> "button"
                 "disabled" ==> vm.buttonDisabled
                 "onclick" ==> fun _ -> dispatch HomeComponent.Add ] [
            str "Add"
        ]
        label [ "class" ==> "label" ] [
            str "Server configuration"
        ]
        input [ "class" ==> "input"
                "placeholder" ==> "Pass-key"
                "value" ==> vm.serverPass
                "onInput"
                ==> fun e -> dispatch (HomeComponent.PasswordChanged e?target?value) ] []
    ]

match document.location.search with
| "?page=list" -> render (comp (ListViewComponent, (), [])) (document.getElementById "root")
| _ -> render (comp (HomeViewComponent, (), [])) (document.getElementById "root")
