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

    let private queue = ResizeArray<byte []>()

    let private handlePost (req: HttpRequest) =
        queue.Add(req.rawForm)
        Successful.NO_CONTENT

    let private handleGet (_: int64) =
        if queue.Count > 0 then
            let c = queue.[0]
            queue.RemoveAt 0
            Successful.ok c
        else
            Successful.NO_CONTENT

    let main () =
        choose [ GET >=> pathScan "/api/%i" handleGet
                 POST >=> path "/api" >=> request handlePost ]
        |> startWebServer defaultConfig

    let receiveEvent (url: string) =
        async {
            use client = new HttpClient()

            let! response = client.GetAsync($"{url}/%i{0}") |> Async.AwaitTask

            let! bytes =
                response.Content.ReadAsByteArrayAsync()
                |> Async.AwaitTask

            let p = FsPickler.CreateBinarySerializer()
            return p.UnPickle bytes
        }

    let sendEvent (url: string) t =
        async {
            let p = FsPickler.CreateBinarySerializer()
            let bytes = p.Pickle t

            use client = new HttpClient()

            do!
                client.PostAsync(url, new ByteArrayContent(bytes))
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
