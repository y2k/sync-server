module Framework

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
    let inline footer props children = h ("footer", createObj props, children)
    let inline span props children = h ("span", createObj props, children)
    let inline select props children = h ("select", createObj props, children)
    let inline option props children = h ("option", createObj props, children)
    let inline label props children = h ("label", createObj props, children)
    let inline ul props children = h ("ul", createObj props, children)
    let inline li props children = h ("li", createObj props, children)
    let inline a props children = h ("a", createObj props, children)

    let inline textarea props children =
        h ("textarea", createObj props, children)

module ElmHooks =
    open Preact

    let useElm handleEvent (init: 'model * 'event list) (update: 'model -> 'msg -> 'event list) =
        let mutable _dispatch: _ -> unit = fun _ -> ()

        let (_model, dispatch) =
            useReducer
                (fun state msg ->
                    let effects = update state msg
                    let mutable newState = state

                    for e in effects do
                        newState <- handleEvent newState _dispatch e

                    newState)
                (fst init)

        _dispatch <- dispatch
        _model, dispatch
