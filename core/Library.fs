namespace SyncServer

module Effect =
    type t =
        interface
        end

    let call (_: t) : _ Async = failwith "???"

open System

type PreferencesEffect =
    | PreferencesEffect of string * string
    interface Effect.t

type 'model UpdateModelEffect =
    | UpdateModelEffect of 'model
    interface Effect.t

type 't AddEffect =
    | AddEffect of server: string * pass: string * title: string * url: string * (Result<unit, exn> -> 't)
    interface Effect.t

module ClientComponent =
    type Model =
        { serverHost: string
          serverPass: string
          title: string
          url: string
          isBusy: bool
          linkType: int
          linkTypes: string [] }
        member this.buttonDisabled = String.IsNullOrEmpty this.url || this.isBusy
        member this.inputDisabled = this.isBusy

    type Msg =
        | ServerHostChanged of string
        | UpdateConfiguration
        | TitleChanged of string
        | UrlChanged of string
        | LinkTypeChanged of int
        | Add
        | AddResult of Result<unit, exn>

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

    let update (prefs: Map<string, string>) (model: Model) (msg: Msg) : Effect.t list =
        match msg with
        | ServerHostChanged value -> [ UpdateModelEffect { model with serverHost = value } ]
        | UpdateConfiguration -> [ PreferencesEffect("server", model.serverHost) ]
        | TitleChanged value -> [ UpdateModelEffect { model with title = value } ]
        | UrlChanged value -> [ UpdateModelEffect { model with url = value } ]
        | LinkTypeChanged value -> [ UpdateModelEffect { model with linkType = value } ]
        | Add ->
            [ UpdateModelEffect { model with isBusy = true }
              AddEffect(model.serverHost, "FIXME", model.title, model.url, AddResult) ]
        | AddResult _ ->
            [ UpdateModelEffect
                  { model with
                      isBusy = false
                      url = ""
                      title = ""
                      linkType = 0 } ]
