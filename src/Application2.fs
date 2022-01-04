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

    let private queue = Atom.atom Map.empty

    let private handlePost (topic: string) (req: HttpRequest) =
        queue.update (fun xs ->
            let q =
                match Map.tryFind topic xs with
                | Some q -> q
                | None -> []

            Map.add topic (q @ [ req.rawForm ]) xs)

        Successful.NO_CONTENT

    let private handleGet ((topic: string), (_: int64)) =
        queue.dispatch (fun xs ->
            match Map.tryFind topic xs with
            | Some (data :: q) -> Map.add topic q xs, Successful.ok data
            | _ -> xs, Successful.NO_CONTENT)

    let startServer () =
        [ GET >=> pathScan "/api/%s/%i" handleGet
          POST
          >=> pathScan "/api/%s" (fun t -> request (handlePost t)) ]
        |> choose
        |> startWebServer defaultConfig

    let rec private getAsyncNotEmpty (url: string) =
        async {
            use client = new HttpClient()

            let! response = client.GetAsync url |> Async.AwaitTask

            let! bytes =
                response.Content.ReadAsByteArrayAsync()
                |> Async.AwaitTask

            if (Array.length bytes > 0) then
                return bytes
            else
                printfn "LOG: sleep 5 sec"
                do! Async.Sleep 5_000
                return! getAsyncNotEmpty url
        }

    let receiveEvent domain (topic: string) =
        async {
            let! bytes = getAsyncNotEmpty $"http://%s{domain}/api/%s{topic}/%i{0}"
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
        { path: string
          content: Blob }
        interface Event

    let handle path (e: NewBlobCreated) : Event list =
        let name = IO.Path.GetFileName e.path

        [ { path = name; content = e.blob }
          { hash = e.blob.hash
            name = name
            tags = e.tags } ]

module FileEventHandlers =
    let foo (e: Downloader.PersistentBlobCreated) = failwith "???"

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
        let private state = Atom.atom { Uploader.Database.dirs = Map.empty }
        let getDb () : Uploader.Database = state.Value
        let resolve f = f (getDb ())

    module ConfigResolver =
        let getDb path : Uploader.Config =
            { server = "localhost"
              sources = [ {| path = path; tags = [] |} ] }

        let resolve path f = f (getDb path)

module Dispatcher =
    let private atomls: (Event -> unit) list Atom.IAtom = Atom.atom []

    let listenUpdate (f: Event -> unit) : unit Async =
        async {
            atomls.update (fun xs -> f :: xs)

            do!
                Async.OnCancel (fun _ ->
                    atomls.update (fun xs ->
                        xs
                        |> List.filter (fun l -> not <| LanguagePrimitives.PhysicalEquality l f)))
                |> Async.Ignore
        }

    let dispatch (e: Event) =
        atomls.Value |> List.iter (fun f -> f e)

    let listenUpdateOnly (f: 'e -> Event list) : unit Async =
        listenUpdate (function
            | :? 'e as e2 ->
                let newEvents = f e2
                newEvents |> List.iter dispatch
            | _ -> ())

module SyncLogic =
    let private start3 () =
        Dispatcher.listenUpdate (fun e ->
            match e with
            | :? NewBlobCreated ->
                RemoteSyncer.sendEvent "localhost:8080" "new-blob-created" e
                |> Async.RunSynchronously
            | _ -> ())

    let private start2 =
        async {
            while true do
                let! (e: NewBlobCreated) = RemoteSyncer.receiveEvent "localhost:8080" "new-blob-created"
                Dispatcher.dispatch e
        }

    let start =
        Async.Parallel [ start3 (); start2 ]
        |> Async.Ignore

let main (args: string array) =
    match args with
    | [| "u" |] ->
        Async.Parallel [ Uploader.handleNewFile
                         |> (Resolvers.ConfigResolver.resolve "../__data/source")
                         |> Resolvers.DatabaseResolver.resolve
                         |> Dispatcher.listenUpdateOnly
                         SyncLogic.start ]
        |> Async.RunSynchronously
        |> ignore
    | [| "d" |] ->
        Async.Parallel [ (Downloader.handle "../__data/target")
                         |> Dispatcher.listenUpdateOnly
                         SyncLogic.start ]
        |> Async.RunSynchronously
        |> ignore
    | [| "s" |] -> RemoteSyncer.startServer ()
    | _ -> ()

    0
