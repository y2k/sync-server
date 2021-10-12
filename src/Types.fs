module Types

open System
open MBrace.FsPickler

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

type FileChangedEvent =
    { data: byte []
      path: string
      date: DateTime }
    static member encode (t: FileChangedEvent) =
        let p = FsPickler.CreateBinarySerializer ()
        p.Pickle t

    static member decode d =
        let p = FsPickler.CreateBinarySerializer ()
        p.UnPickle<FileChangedEvent> d
        |> Some
