module Application

module Types =
    open System

    type UploadConfig =
        { server: string
          key: string
          directories: string list }

    type DownloadConfig = { outDirectory: string; key: string }

    type File = { path: string; lastChanged: DateTime }

    type State =
        { dirs: Map<string, DateTime> }
        static member empty = { dirs = Map.empty }

    type 'a Callback = Callback of obj

    type RemoteType =
        | RemoteString of string
        | RemoteBytes of byte []
        | RemoteDateTime of DateTime

    type EncodeSource =
        | FileSource of path: string
        | BytesSource of data: byte []

    type RemoteCall =
        | GetNewFileCall
        | UploadNewCall
        | RemoveFileCall

    type Effect =
        | LoadFiles of path: string * Callback<string list>
        | EncodeData of encode: bool * data: EncodeSource * key: string * Callback<byte []>
        | RemoteCall of id: RemoteCall * Map<string, RemoteType> * Callback<Map<string, RemoteType>>
        | UpdateDb of db: State
        | SaveFile of path: string * data: byte []

open Types
open System
open Suave
open Suave.Operators
open FsHtml
open Atom
open FSharpx.Collections

type Store =
    { items: PersistentHashMap<int, string> }

let item id url =
    div [ "class" %= "card"
          "style" %= "width: 300px; margin: 4px" ] [
        div [ "class" %= "card-image" ] [
            figure [ "class" %= "image is-4by3" ] [
                img [ "object-fit" %= "cover"
                      "src" %= url
                      "loading" %= "lazy" ] []
            ]
        ]
        div [ "class" %= "card-footer" ] [
            a [ "hx-delete" %= (sprintf "/remove/%i" id)
                "hx-target" %= "body"
                "class" %= "card-footer-item" ] [
                str "Remove"
            ]
            a [ "class" %= "card-footer-item" ] [
                str "Open"
            ]
        ]
    ]

let gallery items =
    div [ "style" %= "display: flex; flex-wrap: wrap" ] [
        for id, url in items |> Seq.sortBy fst do
            item id url
    ]

let content store =
    div [ "style" %= "padding: 4px" ] [
        nav [ "class" %= "pagination"
              "role" %= "navigation"
              "aria-label" %= "pagination" ] [
            ul [ "class" %= "pagination-list" ] [
                li [] [
                    a [ "class" %= "pagination-link"
                        "hx-get" %= "/gallery?id=6966773"
                        "hx-target" %= "body" ] [
                        str "Previous"
                    ]
                ]
                li [] [
                    a [ "class" %= "pagination-link"
                        "hx-get" %= "/gallery?id=6966773"
                        "hx-target" %= "body" ] [
                        str "Next page"
                    ]
                ]
            ]
        ]

        div [ "id" %= "gallery" ] [
            gallery store.items
        ]
    ]

let mkHtml content =
    html [] [
        head [] [
            title [] [ str "Sync server" ]
            link [ "rel" %= "stylesheet"
                   "href"
                   %= "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css" ] []
            script [ "src" %= "https://unpkg.com/htmx.org@1.6.0" ] []
        ]
        body [ "style" %= "display: flex; flex-direction: column" ] [
            content
        ]
    ]

module Application =
    let parseRequest
        (_: Map<string, RemoteType>)
        : {| data: byte []
             filename: string
             date: DateTime |} =
        failwith "???"

    let upload (guid: Guid) (info: Map<string, RemoteType>) =
        let req = parseRequest info
        [ SaveFile($"__store/${guid}", req.data) ]

let testPage = str "TODO" |> ref

let startServer () =
    let store =
        { items =
              Seq.init
                  8
                  (fun i ->
                      let id = 6966772 + i
                      id, sprintf "http://img2.reactor.cc/pics/post/-%i.jpeg" id)
              |> PersistentHashMap.ofSeq }
        |> atom

    let content () = content store.Value

    choose [ Filters.path "/preview"
             >=> request (fun _ -> !testPage |> mkHtml |> toString |> Successful.OK)
             Filters.path "/"
             >=> request (fun _ -> content () |> mkHtml |> toString |> Successful.OK)
             Filters.DELETE
             >=> Filters.pathScan
                     "/remove/%i"
                     (fun id ->
                         store.update
                             (fun db ->
                                 { db with
                                       items = PersistentHashMap.remove id db.items })

                         content () |> toString |> Successful.OK)
             Filters.path "/gallery"
             >=> request
                     (fun r ->
                         let id =
                             r.queryParamOpt "id"
                             |> Option.bind (fun (_, x) -> x)
                             |> Option.defaultValue "6922747"

                         content () |> toString |> Successful.OK) ]
    |> startWebServerAsync defaultConfig
    |> snd

[<EntryPoint>]
let main _ =
    startServer () |> Async.RunSynchronously |> ignore

    0
