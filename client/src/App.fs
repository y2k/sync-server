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

    let useElm (init: 'model) (update: 'msg -> _ -> _) toViewModel =
        let mutable _dispatch: _ -> unit = fun _ -> ()

        let (_model, dispatch) =
            useReducer
                (fun state msg ->
                    let (newState, effects) = update msg state

                    async {
                        for e in effects do
                            let! m = e
                            _dispatch m
                    }
                    |> Async.StartImmediate

                    newState)
                init

        _dispatch <- dispatch
        let vm = toViewModel _model
        vm, dispatch

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

module ViewDomain =
    let addItem (_server: string) (_pass: string) (_title: string) (_url: string) : unit Async = Async.Sleep 1_000

    type Model =
        { serverHost: string
          serverPass: string
          title: string
          url: string
          isBusy: bool
          linkType: int
          linkTypes: string [] }

    type Msg =
        | ServerHostChanged of string
        | UpdateConfiguration
        | TitleChanged of string
        | UrlChanged of string
        | LinkTypeChanged of int
        | Add
        | AddResult of Result<unit, exn>

    let viewModel (model: Model) =
        {| serverHost = model.serverHost
           url = model.url
           title = model.title
           inputDisabled = model.isBusy
           buttonDisabled = String.IsNullOrEmpty model.url || model.isBusy
           linkType = model.linkType
           linkTypes = model.linkTypes |}

    let init =
        { serverHost = ""
          serverPass = ""
          url = ""
          title = ""
          isBusy = false
          linkType = 0
          linkTypes =
            [| "URL (bookmark)"
               "Watch late (youtube)" |] }

    let update (prefs: Map<string, string>) (msg: Msg) (model: Model) =
        match msg with
        | ServerHostChanged value -> { model with serverHost = value }, []
        | UpdateConfiguration ->
            model,
            [ Preferences.savePreference "server" model.serverHost
              |> failwith "???" ]
        | TitleChanged value -> { model with title = value }, []
        | UrlChanged value -> { model with url = value }, []
        | LinkTypeChanged value -> { model with linkType = value }, []
        | Add ->
            { model with isBusy = true },
            [ Async.catchWith AddResult (addItem model.serverHost (Map.find "server" prefs) model.title model.url) ]
        | AddResult _ ->
            { model with
                isBusy = false
                url = ""
                title = ""
                linkType = 0 },
            []

open Browser.Dom
open Fable.Core.JsInterop
open Preact

let HomeComponent (props: _) =
    let (vm, dispatch) =
        ElmHooks.useElm ViewDomain.init (Preferences.decorate ViewDomain.update) ViewDomain.viewModel

    div [ "class" ==> "form" ] [
        input [ "class" ==> "input"
                "placeholder" ==> "URL"
                "value" ==> vm.url
                "disabled" ==> vm.inputDisabled
                "onInput"
                ==> fun e -> dispatch (ViewDomain.UrlChanged e?target?value) ] []
        textarea [ "class" ==> "textarea"
                   "placeholder" ==> "Title"
                   "value" ==> vm.title
                   "disabled" ==> vm.inputDisabled
                   "onInput"
                   ==> fun e -> dispatch (ViewDomain.TitleChanged e?target?value) ] []
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
                          ==> fun e -> dispatch (ViewDomain.LinkTypeChanged e?target?value)
                          "disabled" ==> vm.inputDisabled ]
                        (Array.mapi (fun i x -> option [ "value" ==> i ] [ str x ]) vm.linkTypes)
                ]
            ]
        ]
        button [ "class" ==> "button"
                 "disabled" ==> vm.buttonDisabled
                 "onclick" ==> fun _ -> dispatch ViewDomain.Add ] [
            str "Add"
        ]
        label [ "class" ==> "label" ] [
            str "Server configuration"
        ]
        input [ "class" ==> "input"
                "placeholder" ==> "Server host"
                "value" ==> vm.serverHost
                "onInput"
                ==> fun e -> dispatch (ViewDomain.ServerHostChanged e?target?value) ] []
        input [ "class" ==> "input"
                "placeholder" ==> "Pass-key"
                "value" ==> vm.url ] []
        button [ "class" ==> "button"
                 "onclick"
                 ==> fun _ -> dispatch ViewDomain.UpdateConfiguration ] [
            str "Update"
        ]
    ]

render (comp (HomeComponent, (), [])) (document.getElementById "root")
