namespace SyncServer

open System

module Password =
    let expand pass =
        sprintf "0c5d760951d4%s3113da07dc06" pass

module Message =
    type Mode =
        | Url
        | Watchlate

    type t =
        { title: string
          url: string
          mode: Mode }

    let decode (data: byte []) : t =
        let s = Text.Encoding.UTF8.GetString data
        let parts = s.Split('&')

        { url = Uri.UnescapeDataString parts.[0]
          title = Uri.UnescapeDataString parts.[1]
          mode = Url }

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

type NavigationChanged =
    | NavigationChanged of string
    interface Event

module NavigationComponent =
    type Msg =
        | HomeClicked
        | ListClicked
        | PictureClicked

    let update msg : Event list =
        match msg with
        | HomeClicked -> [ NavigationChanged "home" ]
        | ListClicked -> [ NavigationChanged "list" ]
        | PictureClicked -> [ NavigationChanged "pic" ]

type 'model ModelChanged =
    | ModelChanged of 'model
    interface Event

module PictureComponent =
    type Model =
        { username: string
          password: string
          path: string }

    type Msg =
        | NavigationClicked of NavigationComponent.Msg
        | UsernameChanged of string
        | PasswordChanged of string
        | PathChanged of string

    let init =
        { username = ""
          password = ""
          path = "" },
        []

    let update model msg =
        match msg with
        | NavigationClicked m -> NavigationComponent.update m
        | PathChanged path -> [ ModelChanged { model with path = path } ]
        | _ -> []

type 't MessagesRequested =
    | MessagesRequested of username: string * server: string * pass: string * (Result<byte [] list, exn> -> 't)
    interface Event

module ListComponent =
    type Item = { url: string; title: string }

    type Model =
        { username: string
          pass: string
          items: Item [] }
        member this.buttonDisabled =
            String.IsNullOrEmpty this.username
            || String.IsNullOrEmpty this.pass

    type Msg =
        | NavigationClicked of NavigationComponent.Msg
        | PasswordChanged of string
        | LoadMessagesClicked
        | MessagesLoaded of Result<byte [] list, exn>
        | UsernameChanged of string

    let init: Model * Event list =
        { username = ""
          pass = ""
          items = [||] },
        []

    let update (model: Model) (msg: Msg) : Event list =
        match msg with
        | NavigationClicked m -> NavigationComponent.update m
        | UsernameChanged value -> [ ModelChanged { model with username = value } ]
        | PasswordChanged value -> [ ModelChanged { model with pass = value } ]
        | LoadMessagesClicked ->
            [ MessagesRequested(model.username, "/api/history-get", Password.expand model.pass, MessagesLoaded) ]
        | MessagesLoaded (Ok payloads) ->
            let items =
                payloads
                |> List.map Message.decode
                |> List.map (fun i -> { url = i.url; title = i.title })
                |> List.toArray

            [ ModelChanged { model with items = items } ]
        | MessagesLoaded (Error _) -> []

type PreferencesEffect =
    | PreferencesEffect of string * string
    interface Event

type 't NewMessageCreated =
    | NewMessageCreated of
        username: string *
        server: string *
        pass: string *
        payload: byte [] *
        (Result<unit, exn> -> 't)
    interface Event

module HomeComponent =
    type Model =
        { serverHost: string
          serverPass: string
          username: string
          title: string
          url: string
          isBusy: bool
          linkType: int
          linkTypes: string [] }
        member this.buttonDisabled =
            String.IsNullOrEmpty this.url
            || this.isBusy
            || String.IsNullOrEmpty this.serverPass
            || String.IsNullOrEmpty this.username

        member this.inputDisabled = this.isBusy

    type Msg =
        | ServerHostChanged of string
        | PasswordChanged of string
        | TitleChanged of string
        | UrlChanged of string
        | LinkTypeChanged of int
        | UsernameChanged of string
        | Add
        | AddResult of Result<unit, exn>
        | NavigationClicked of NavigationComponent.Msg

    let init: Model * Event list =
        { serverHost = ""
          serverPass = ""
          username = ""
          url = ""
          title = ""
          isBusy = false
          linkType = 0
          linkTypes =
            [| "URL (bookmark)"
               "Watch late (youtube)" |] },
        []

    let update (prefs: Map<string, string>) (model: Model) (msg: Msg) : Event list =
        match msg with
        | NavigationClicked m -> NavigationComponent.update m
        | UsernameChanged value -> [ ModelChanged { model with username = value } ]
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
              NewMessageCreated(
                  model.username,
                  "/api/history-add",
                  Password.expand model.serverPass,
                  payload,
                  AddResult
              ) ]
        | AddResult _ ->
            [ ModelChanged
                  { model with
                      isBusy = false
                      url = ""
                      title = "" } ]
