module Application

open System
open Types

module DownloadApp =
    type ISaveFiles =
        abstract member invoke: string -> byte [] -> unit

    let main env (config: DownloadConfig) event =
        match FileChangedEvent.decode event with
        | None -> ()
        | Some info -> (env :> ISaveFiles).invoke $"{config.outDirectory}/{info.path}" info.data

module UploadApp =
    type ISendEvent =
        abstract member invoke: byte [] -> unit

    type IReadAllBytes =
        abstract member invoke: string -> byte array

    type IGetDb =
        abstract member invoke: unit -> State

    type IGetFiles =
        abstract member invoke: string -> File list

    let private uploadFile env (file: File) =
        let data = (env :> IReadAllBytes).invoke file.path

        { data = data
          path = Utils.getFilename file.path
          date = file.lastChanged }
        |> FileChangedEvent.encode
        |> (env :> ISendEvent).invoke

    let private getNewFiles db dir files =
        let lastChanged =
            db.dirs
            |> Map.tryFind dir
            |> Option.defaultValue DateTime.MinValue

        files
        |> List.filter (fun file -> file.lastChanged > lastChanged)

    let private uploadNewFiles env dir (files: File list) =
        let db = (env :> IGetDb).invoke ()

        getNewFiles db dir files
        |> Seq.iter (uploadFile env)

    let private handleDirectory env dir =
        (env :> IGetFiles).invoke dir
        |> List.ofSeq
        |> uploadNewFiles env dir

    let main env config =
        for dir in config.directories do
            handleDirectory env dir

module Server =
    open Suave
    open Suave.Operators

    let private handleGet (_offset: int64) = failwith "???"

    let private handlePost (_req: HttpRequest) = failwith "???"

    let main _ =
        choose [ Filters.GET
                 >=> Filters.pathScan "/api/%i" handleGet
                 Filters.POST
                 >=> Filters.path "/api"
                 >=> request handlePost ]
        |> startWebServer defaultConfig

[<EntryPoint>]
let main args = Application2.main args
