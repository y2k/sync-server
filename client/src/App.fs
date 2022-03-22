module App

open System

module Preact =
    open Fable.Core
    open Fable.Core.JsInterop

    type VNode =
        interface
        end

    let inline str (x: string) : VNode = !!x

    [<Import("useReducer", "preact/hooks")>]
    let useReducer (_: 'state -> 'action -> 'state) (_: 'state) : 'state * ('action -> unit) = jsNative

    [<Import("useState", "preact/hooks")>]
    let useState (_: 't) : ('t * (('t -> 't) -> unit)) = jsNative

    [<Import("useEffect", "preact/hooks")>]
    let useEffect (_: Func<unit, unit -> unit>) (_: _ array) : unit = jsNative

    [<Import("h", "preact")>]
    let h (_: string, _: 'prop, [<ParamArray>] children: VNode seq) : VNode = jsNative

    [<Import("h", "preact")>]
    let comp (_: _ -> VNode, _: 'prop, [<ParamArray>] children: VNode seq) : VNode = jsNative

    [<Import("render", "preact")>]
    let render (_: VNode) (node: obj) = jsNative

    let inline button props children = h ("button", createObj props, children)
    let inline input props children = h ("input", createObj props, children)
    let inline form props children = h ("form", createObj props, children)
    let inline div props children = h ("div", createObj props, children)
    let inline span props children = h ("span", createObj props, children)
    let inline select props children = h ("select", createObj props, children)
    let inline option props children = h ("option", createObj props, children)
    let inline label props children = h ("label", createObj props, children)

    let inline textarea props children =
        h ("textarea", createObj props, children)

module ElmHooks =
    open Preact
    open SyncServer
    open Browser.Dom

    let useElm (init: 'model * Event list) (update: 'model -> 'msg -> Event list) =
        let mutable _dispatch: _ -> unit = fun _ -> ()

        let handleEvents (events: Event list) =
            for e in events do
                match e with
                | :? MessagesRequested<'msg> as pe ->
                    let (MessagesRequested (server, pass, next)) = pe

                    async {
                        let! response = MessageUploder.loadAllPayloads server pass
                        let m = next (Ok response)
                        _dispatch m
                    }
                    |> Async.StartImmediate
                | _ -> ()

        let (_model, dispatch) =
            useReducer
                (fun state msg ->
                    let effects = update state msg
                    let mutable newState = state

                    for e in effects do
                        match e with
                        | :? MessagesRequested<'msg> as pe ->
                            let (MessagesRequested (server, pass, next)) = pe

                            async {
                                let! response = MessageUploder.loadAllPayloads server pass
                                let m = next (Ok response)
                                _dispatch m
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
                                _dispatch m
                            }
                            |> Async.StartImmediate
                        | _ -> ()

                    newState)
                (fst init)

        _dispatch <- dispatch

        handleEvents (snd init)

        _model, dispatch

module Async =
    let catchWith f a =
        async {
            try
                let! r = a
                return f (Ok r)
            with
            | e -> return f (Error e)
        }

module Preferences =
    let mutable private store: Map<string, string> = Map.empty

    let savePreference (key: string) (value: string) : unit Async =
        async { store <- Map.add key value store }

    let decorate (f: Map<string, string> -> _) = f store

open Browser.Dom
open Fable.Core.JsInterop
open Preact
open SyncServer

let ListViewComponent (props: _) =
    let (model, dispatch) = ElmHooks.useElm ListComponent.init ListComponent.update

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
        ElmHooks.useElm HomeComponent.init (Preferences.decorate HomeComponent.update)

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
