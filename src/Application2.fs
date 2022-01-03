module Application2

open System

type Event =
    interface
    end

type NewFileCreated =
    { path: string
      dir: string
      lastChanged: DateTime }
    interface Event

module FileWatcher =
    let handle (dir: string) =
        IO.Directory.GetFiles dir
        |> Seq.map (fun path ->
            { path = path
              dir = dir
              lastChanged = IO.File.GetLastWriteTime(path) })

type Blob =
    { path: string
      data: byte [] option
      hash: byte [] }

type NewBlobCreated =
    { path: string
      tags: string list
      blob: Blob }
    interface Event

module RemoteSyncer =
    open System.Net.Http
    open MBrace.FsPickler
    open Suave
    open Suave.Filters
    open Suave.Operators

    let private queue = ResizeArray<string * byte []>()

    let private handlePost (topic : string) (req: HttpRequest) =
        queue.Add(topic, req.rawForm)
        Successful.NO_CONTENT

    let private handleGet ((topic : string), (_: int64)) =
        match Seq.tryFindIndex (fun (t, _) -> t = topic) queue with
        | Some index ->
            let (_, c) = queue.[index]
            queue.RemoveAt index
            Successful.ok c
        | None -> Successful.NO_CONTENT

    let startServer () =
        [ GET >=> pathScan "/api/%s/%i" handleGet
          POST >=> pathScan "/api/%s" (fun t -> request (handlePost t)) ]
        |> choose
        |> startWebServer defaultConfig

    let receiveEvent domain (topic: string) =
        async {
            use client = new HttpClient()

            let! response = client.GetAsync($"http://%s{domain}/api/%s{topic}/%i{0}") |> Async.AwaitTask

            let! bytes =
                response.Content.ReadAsByteArrayAsync()
                |> Async.AwaitTask

            let p = FsPickler.CreateBinarySerializer()
            return p.UnPickle bytes
        }

    let sendEvent domain (topic: string) t =
        async {
            let p = FsPickler.CreateBinarySerializer()
            let bytes = p.Pickle t

            use client = new HttpClient()

            do!
                client.PostAsync($"http://%s{domain}/api/%s{topic}", new ByteArrayContent(bytes))
                |> Async.AwaitTask
                |> Async.Ignore
        }

module Downloader =
    type PersistentEventCreated =
        { hash: byte []
          name: string
          tags: string list }
        interface Event

    type PersistentBlobCreated =
        { name: string
          content: Blob }
        interface Event

    let handle (e: NewBlobCreated) : Event list =
        let name = IO.Path.GetFileName e.path

        [ { name = name; content = e.blob }
          { hash = e.blob.hash
            name = name
            tags = e.tags } ]

module Uploader =
    type Config =
        { server: string
          sources: {| path: string; tags: string list |} list }

    type Database = { dirs: Map<string, DateTime> }

    let getLastChangedForDir db dir =
        Map.tryFind dir db.dirs
        |> Option.defaultValue DateTime.MinValue

    type DatabaseChanged =
        { db: Database }
        interface Event

    let getTags (config: Config) dir =
        config.sources
        |> Seq.tryPick (fun x ->
            if x.path = dir then
                Some x.tags
            else
                None)
        |> Option.defaultValue []

    let handleNewFile (config: Config) (db: Database) (event: NewFileCreated) : Event list =
        let lastChanged = getLastChangedForDir db event.dir

        if lastChanged > event.lastChanged then
            [ { path = event.path
                tags = getTags config event.dir
                blob =
                  { path = event.path
                    data = None
                    hash = [||] } }
              { db = db } ]
        else
            []

module Resolvers =
    module DatabaseResolver =
        let getDb () : Uploader.Database = failwith "???"
        let resolve f = f (getDb ())

    module ConfigResolver =
        let getDb () : Uploader.Config =
            { server = ""
              sources = [ {| path = ""; tags = [] |} ] }

        let resolve f = f (getDb ())

module Dispatcher =
    let listenUpdate (_: #Event -> Event list) : unit Async = failwith "???"
    let dispatch (_: Event) : unit = failwith "???"

let main (args : string array) =
    match args with
    | [| "uploader" |] ->
        Uploader.handleNewFile
        |> Resolvers.ConfigResolver.resolve
        |> Resolvers.DatabaseResolver.resolve
        |> Dispatcher.listenUpdate
        |> Async.RunSynchronously
        |> ignore
    | [| "downloader" |] -> failwith "???"
    | [| "server" |] -> failwith "???"
    | _ -> ()
    0
