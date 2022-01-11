module Applicaion

open System

type Event =
    interface
    end

type NewFileCreated =
    { path: string
      dir: string
      lastChanged: DateTime }
    interface Event

open MBrace.FsPickler

[<CustomPickler>]
type Blob =
    { path: string
      data: byte [] option }
    static member CreatePickler(resolver: IPicklerResolver) =
        let sr = resolver.Resolve<string>()
        let ar = resolver.Resolve<byte array>()

        Pickler.FromPrimitives(
            (fun rs ->
                { path = sr.Read rs "path"
                  data = Some(ar.Read rs "data") }),
            (fun ws c ->
                sr.Write ws "path" c.path

                match c.data with
                | Some d -> ar.Write ws "data" d
                | None -> ar.Write ws "data" (IO.File.ReadAllBytes c.path))
        )

type NewBlobCreated =
    { path: string
      category: string
      tags: string list
      blob: Blob option }
    interface Event

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

    let private getTags (config: Config) dir =
        config.sources
        |> Seq.tryPick (fun x ->
            if x.path = dir then
                Some x.tags
            else
                None)
        |> Option.defaultValue []

    let private updateDirLastChanged (db: Database) path (date: DateTime) =
        { db with dirs = Map.add path date db.dirs }

    let handleNewFile (config: Config) (db: Database) (event: NewFileCreated) : Event list =
        let lastChanged = getLastChangedForDir db event.dir

        if event.lastChanged > lastChanged then
            [ { path = event.path
                category = "video"
                tags = getTags config event.dir
                blob = Some { path = event.path; data = None } }
              { db = updateDirLastChanged db event.dir event.lastChanged } ]
        else
            []

module Downloader =
    type RdfItemCreated =
        { attrs: (string * string) list }
        interface Event

    type PersistentBlobCreated =
        { path: string
          content: Blob }
        interface Event

    let handle path (e: NewBlobCreated) : Event list =
        let name = IO.Path.GetFileName e.path
        let locPath = IO.Path.Combine(path, name)

        [ yield
              { attrs =
                  [ "name", name
                    "type", e.category
                    yield! e.tags |> List.map (fun t -> "tag", t) ] }
          match e.blob with
          | Some blob -> yield { path = locPath; content = blob }
          | None -> () ]

//
//
//

module AdminServer =
    open FsHtml

    let handleAddUrl (form: (string * string option) list) : Event list =
        let fm =
            form
            |> Seq.map (fun (k, v) -> k, v |> Option.defaultValue "")
            |> Map.ofSeq

        match List.tryFind (fun (k, _) -> k = "url") form with
        | Some (_, Some url) ->
            [ { path = url
                category = "url"
                tags = []
                blob = None } ]
        | _ -> []

    let main =
        html [] [
            head [] [
                title [] [ str "Control panel" ]
                link [ "rel" %= "stylesheet"
                       "href"
                       %= "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css" ] []
                style [] [
                    """
.form { display: flex; flex-direction: column; width: 500px; margin: auto; padding-top: 16px }
.form > *:not(:last-child) { margin-bottom: 16px; }
                    """
                    |> str
                ]
            ]
            body [] [
                form [ "class" %= "form"
                       "method" %= "post" ] [
                    // h1 [] [ str "Add LINK" ]
                    input [ "class" %= "input"
                            "placeholder" %= "URL"
                            "name" %= "url" ] []
                    input [ "class" %= "input"
                            "placeholder" %= "Title"
                            "name" %= "title" ] []
                    button [ "class" %= "button" ] [
                        str "Add"
                    ]
                ]
            ]
        ]
        |> toString

module RemoteSyncer =
    open System.Net.Http
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

    let private logRoute next =
        request (fun r ->
            printfn "LOG: request = %O" r
            succeed)
        >=> next

    let startServer pass dispatch =
        [ GET >=> pathScan "/api/%s/%i" handleGet
          POST
          >=> pathScan "/api/%s" (fun t -> request (handlePost t))
          Authentication.authenticateBasic
              ((=) ("admin", pass))
              (choose [ POST
                        >=> path "/admin"
                        >=> request (fun r ->
                            dispatch (AdminServer.handleAddUrl r.form)
                            Successful.OK AdminServer.main)
                        GET
                        >=> path "/admin"
                        >=> Successful.OK AdminServer.main ]) ]
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

    open System.IO
    open System.Security.Cryptography
    open System.Text

    let private makeAes (key: string) =
        Aes.Create(Key = MD5.HashData(Encoding.UTF8.GetBytes(key)), IV = Array.zeroCreate 16)

    let private encrypt (key: string) (data: byte []) =
        use aes = makeAes key
        use outStream = new MemoryStream()

        use cryptStream =
            new CryptoStream(outStream, aes.CreateEncryptor(aes.Key, aes.IV), CryptoStreamMode.Write)

        cryptStream.Write(data, 0, data.Length)
        cryptStream.FlushFinalBlock()
        outStream.ToArray()

    let private decrypt (key: string) (data: byte []) =
        use aes = makeAes key
        use inStream = new MemoryStream(data)
        use outStream = new MemoryStream()

        use cryptStream =
            new CryptoStream(inStream, aes.CreateDecryptor(aes.Key, aes.IV), CryptoStreamMode.Read)

        cryptStream.CopyTo(outStream)
        outStream.ToArray()

    let receiveEvent domain password (topic: string) =
        async {
            let! bytes = getAsyncNotEmpty $"http://%s{domain}/api/%s{topic}/%i{0}"
            let p = FsPickler.CreateBinarySerializer()
            return p.UnPickle(decrypt password bytes)
        }

    let sendEvent domain password (topic: string) t =
        async {
            let p = FsPickler.CreateBinarySerializer()
            let bytes = p.Pickle t

            use client = new HttpClient()
            let content = new ByteArrayContent(encrypt password bytes)

            do!
                client.PostAsync($"http://%s{domain}/api/%s{topic}", content)
                |> Async.AwaitTask
                |> Async.Ignore
        }

module Resolvers =
    module DatabaseResolver =
        let private state = Atom.atom { Uploader.Database.dirs = Map.empty }
        let getDb () : Uploader.Database = state.Value
        let decorate f args = f (getDb ()) args
        let handle (e: Uploader.DatabaseChanged) = state.update (fun _ -> e.db)

    module ConfigResolver =
        let getDb path : Uploader.Config =
            { server = "localhost"
              sources = [ {| path = path; tags = [] |} ] }

        let resolve path f = f (getDb path)

module Dispatcher =
    let private atomls: (Event -> unit) list Atom.IAtom = Atom.atom []

    let listenUpdates (f: Event -> unit) =
        async {
            atomls.update (fun xs -> f :: xs)

            use! __ =
                Async.OnCancel (fun _ ->
                    atomls.update (fun xs ->
                        xs
                        |> List.filter (fun l -> not <| LanguagePrimitives.PhysicalEquality l f)))

            do! Async.Sleep -1
        }

    let dispatch (e: Event) =
        atomls.Value |> List.iter (fun f -> f e)

    let handleEvent (f: 'e -> Event list) (e: Event) =
        match e with
        | :? 'e as e2 ->
            let newEvents = f e2
            newEvents |> List.iter dispatch
        | _ -> ()

module FileWatcher =
    let handle (dir: string) =
        async {
            do! Async.Sleep 500

            IO.Directory.GetFiles dir
            |> Seq.map (fun path ->
                { path = path
                  dir = dir
                  lastChanged = IO.File.GetLastWriteTime(path) })
            |> Seq.iter Dispatcher.dispatch
        }

module SyncLogic =
    let sendEventsToServer pass =
        Dispatcher.listenUpdates (fun e ->
            match e with
            | :? NewBlobCreated ->
                RemoteSyncer.sendEvent "localhost:8080" pass "new-blob-created" e
                |> Async.RunSynchronously
            | _ -> ())

    let getEventsFromServer pass =
        async {
            while true do
                let! (e: Event) = RemoteSyncer.receiveEvent "localhost:8080" pass "new-blob-created"
                Dispatcher.dispatch e
        }

module LogEventHandler =
    let handle _prefix (e: Event) =
        printfn "(EVENT) %s\n%O\n" (e.GetType().Name) e

module RdfStorage =
    open Microsoft.Data.Sqlite
    open Dapper

    [<CLIMutable>]
    type Rdf =
        { subject_id: int64
          relation: string
          object_text: string }

    let handle dbpath =
        let conn = new SqliteConnection $"DataSource=%s{dbpath}"
        conn.Open()

        conn.Execute
            """CREATE TABLE IF NOT EXISTS main (
            subject_id integer NOT NULL,
            relation text,
            relation_id integer,
            object_text text,
            object_id integer
          )"""
        |> ignore

        fun (e: Downloader.RdfItemCreated) ->
            use t = conn.BeginTransaction()

            let subjectId =
                conn.QueryFirstOrDefault<int64>("SELECT subject_id FROM main ORDER BY subject_id DESC LIMIT 1")
                + 1L

            for (relation, object) in e.attrs do
                conn.Execute(
                    "INSERT INTO main (subject_id, relation, object_text) VALUES (@subject_id, @relation, @object_text)",
                    { subject_id = subjectId
                      relation = relation
                      object_text = object }
                )
                |> ignore

            t.Commit()

module FileDispatcher =
    let handle (e: Downloader.PersistentBlobCreated) =
        IO.File.WriteAllBytes(e.path, e.content.data |> Option.get)

[<EntryPoint>]
let main args =
    let pass = "7e1195e0bbd0"

    match args with
    | [| "u" |] ->
        Async.Parallel [ Uploader.handleNewFile
                         |> (Resolvers.ConfigResolver.resolve "__data/source")
                         |> Resolvers.DatabaseResolver.decorate
                         |> Dispatcher.handleEvent
                         |> Dispatcher.listenUpdates
                         (Resolvers.DatabaseResolver.handle >> (fun _ -> []))
                         |> Dispatcher.handleEvent
                         |> Dispatcher.listenUpdates
                         FileWatcher.handle "__data/source"
                         SyncLogic.sendEventsToServer pass
                         LogEventHandler.handle "upload"
                         |> Dispatcher.listenUpdates ]
        |> Async.RunSynchronously
        |> ignore
    | [| "d" |] ->
        Async.Parallel [ Downloader.handle "__data/target"
                         |> Dispatcher.handleEvent
                         |> Dispatcher.listenUpdates
                         (RdfStorage.handle "__data/rdf.db" >> (fun _ -> []))
                         |> Dispatcher.handleEvent
                         |> Dispatcher.listenUpdates
                         (FileDispatcher.handle >> (fun _ -> []))
                         |> Dispatcher.handleEvent
                         |> Dispatcher.listenUpdates
                         SyncLogic.getEventsFromServer pass
                         LogEventHandler.handle "download"
                         |> Dispatcher.listenUpdates ]
        |> Async.RunSynchronously
        |> ignore
    | [| "s" |] ->
        Async.Parallel [ async { RemoteSyncer.startServer pass (List.iter Dispatcher.dispatch) }
                         LogEventHandler.handle ()
                         |> Dispatcher.listenUpdates ]
        |> Async.RunSynchronously
        |> ignore
    | _ -> ()

    0
