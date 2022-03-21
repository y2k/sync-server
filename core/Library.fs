namespace SyncServer

open System

module Message =
    type Mode =
        | Url
        | Watchlate

    let make (title: string) (url: string) (mode: Mode) =
        let url = Uri.EscapeDataString url
        let title = Uri.EscapeDataString title

        let mode =
            match mode with
            | Url -> 0
            | Watchlate -> 1

        $"%s{url}&%s{title}&%i{mode}"
        |> Text.Encoding.UTF8.GetBytes

type Event =
    interface
    end

module ListComponent =
    type Model = Model
    type Msg = Msg
    let update _ _ = []

type PreferencesEffect =
    | PreferencesEffect of string * string
    interface Event

type 'model ModelChanged =
    | ModelChanged of 'model
    interface Event

type 't NewMessageCreated =
    | NewMessageCreated of server: string * pass: string * payload: byte [] * (Result<unit, exn> -> 't)
    interface Event

type NavigationChanged =
    | NavigationChanged of string
    interface Event

module ClientComponent =
    type Model =
        { serverHost: string
          serverPass: string
          title: string
          url: string
          isBusy: bool
          linkType: int
          linkTypes: string [] }
        member this.buttonDisabled =
            String.IsNullOrEmpty this.url
            || this.isBusy
            || String.IsNullOrEmpty this.serverPass

        member this.inputDisabled = this.isBusy

    type Msg =
        | ServerHostChanged of string
        | PasswordChanged of string
        | TitleChanged of string
        | UrlChanged of string
        | LinkTypeChanged of int
        | Add
        | AddResult of Result<unit, exn>
        | ListClicked

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

    let update (prefs: Map<string, string>) (model: Model) (msg: Msg) : Event list =
        match msg with
        | ListClicked -> [ NavigationChanged "list" ]
        | ServerHostChanged value -> [ ModelChanged { model with serverHost = value } ]
        | PasswordChanged value -> [ ModelChanged { model with serverPass = value } ]
        | TitleChanged value -> [ ModelChanged { model with title = value } ]
        | UrlChanged value -> [ ModelChanged { model with url = value } ]
        | LinkTypeChanged value -> [ ModelChanged { model with linkType = value } ]
        | Add ->
            let mode =
                match model.linkType with
                | 0 -> Message.Url
                | _ -> Message.Watchlate

            let payload = Message.make model.title model.url mode

            [ ModelChanged { model with isBusy = true }
              NewMessageCreated("/api/history", model.serverPass, payload, AddResult) ]
        | AddResult _ ->
            [ ModelChanged
                  { model with
                      isBusy = false
                      url = ""
                      title = ""
                      linkType = 0 } ]
