module Tests

open System
open Xunit
open Swensen.Unquote
open Types
open Application

let now = DateTime.Now

type MockEffect (eventRef : byte [] option ref,
                 saveRef : (string * byte []) list ref) =
    interface DownloadApp.ISaveFiles with
        member _.invoke path bytes = saveRef := [ path, bytes ]
    interface UploadApp.IReadAllBytes with
        member _.invoke _ = Text.Encoding.UTF8.GetBytes "hello"
    interface UploadApp.IGetFiles with
        member _.invoke _ = [ { path = "/root/file.txt"; lastChanged = now } ]
    interface UploadApp.IGetDb with
        member _.invoke () = State.empty
    interface UploadApp.ISendEvent with
        member _.invoke event = eventRef := Some event

[<Fact>]
let ``upload new files`` () =
    let actual: byte [] option ref = ref None
    UploadApp.main
        (MockEffect (actual, ref []))
        { server = ""; key = "key"; directories = [ "/root/" ] }

    let expected =
        { data = Convert.FromBase64String "aGVsbG8="
          path = "file.txt"
          date = now }
        |> FileChangedEvent.encode
    let actual = !actual |> Option.get
    test <@ expected = actual @>

[<Fact>]
let ``download new file`` () =
    let actual = ref []
    DownloadApp.main
        (MockEffect (ref None, actual))
        { outDirectory = "/home"; key = "key" }
        ({ date = now
           path = "file.txt"
           data = Convert.FromBase64String "aGVsbG8="
         } |> FileChangedEvent.encode)

    let expected = "/home/file.txt", [| 104uy; 101uy; 108uy; 108uy; 111uy |]
    let actual = !actual |> List.head
    test <@ expected = actual @>
