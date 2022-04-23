module Applicaion

open System

module RemoteSyncer =
    open System.IO
    open System.Net
    open Suave
    open Suave.Filters
    open Suave.Operators

    let private handlePost insertBlob (req: HttpRequest) =
        printfn "LOG: headers: %A" req.headers
        let topic = req.headers |> Map.ofSeq |> Map.find "x-session"
        insertBlob topic req.rawForm
        Successful.NO_CONTENT

    let private handleGet getNextById (req: HttpRequest) =
        let topic = req.headers |> Map.ofSeq |> Map.find "x-session"

        let id =
            req.query
            |> Map.ofSeq
            |> Map.tryFind "prevId"
            |> Option.flatten
            |> Option.map int64
            |> Option.defaultValue 0

        match getNextById topic id with
        | [] -> Successful.NO_CONTENT
        | items ->
            let buffer =
                items
                |> List.fold
                    (fun (buf: MemoryStream) (data: byte [], id: int64) ->
                        buf.Write(BitConverter.GetBytes(data.Length), 0, 4)
                        buf.Write(data, 0, data.Length)
                        buf)
                    (new MemoryStream())

            Successful.ok (buffer.ToArray())
            >=> Writers.setHeader "X-ID" (items |> List.last |> snd |> string)

    let private logRoute next =
        request (fun r ->
            printfn "LOG: request = %O" r
            succeed)
        >=> next

    let startServer insertBlob getNextById =
        let config =
            { defaultConfig with bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") 8080us ] }

        [ GET
          >=> path "/healthcheck"
          >=> Successful.OK "success"
          GET
          >=> path "/"
          >=> request (fun _ ->
              let path = "../web/public/index.html"
              let html = IO.File.ReadAllText path

              html.Replace("BUNDLE_HASHCODE", File.GetCreationTimeUtc(path).Ticks.ToString())
              |> Successful.OK)
          GET
          >=> path "/index.css"
          >=> Files.file "../web/public/index.css"
          GET
          >=> path "/favicon.svg"
          >=> Files.file "../web/public/favicon.svg"
          GET
          >=> path "/bundle.js"
          >=> Files.file "../web/public/bundle.js"
          POST
          >=> path "/api/history-get"
          >=> request (handleGet getNextById)
          POST
          >=> path "/api/history-add"
          >=> request (handlePost insertBlob) ]
        |> choose
        |> startWebServerAsync config
        |> snd

module EventStorage =
    open Microsoft.Data.Sqlite
    open Dapper

    [<CLIMutable>]
    type NewItemRead = { id: int64; data: byte [] }

    type t = private { conn: SqliteConnection }

    let make dbpath =
        let conn = new SqliteConnection $"DataSource=%s{dbpath}"
        conn.Open()

        conn.Execute
            """CREATE TABLE IF NOT EXISTS main (
            topic TEXT,
            data BLOB
          )"""
        |> ignore

        { conn = conn }

    let getNextById (t: t) (topic: string) (id: int64) : (byte [] * int64) list =
        t.conn.Query<NewItemRead>(
            "SELECT rowid AS id, data FROM main WHERE topic = @topic AND rowid > @id ORDER BY rowid LIMIT 50",
            {| id = id; topic = topic |}
        )
        |> Seq.map (fun x -> x.data, x.id)
        |> Seq.toList

    let insert t (topic: string) (data: byte []) =
        use tran = t.conn.BeginTransaction()

        t.conn.Execute("INSERT INTO main (topic, data) VALUES (@topic, @data)", {| topic = topic; data = data |})
        |> ignore

        tran.Commit()

[<EntryPoint>]
let main _ =
    let db = EventStorage.make "__data/message.db"
    RemoteSyncer.startServer (EventStorage.insert db) (EventStorage.getNextById db)
    |> Async.RunSynchronously
    |> ignore
    0
