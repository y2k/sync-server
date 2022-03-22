module MessageUploder

module Crypto =
    open Fable.Core
    open Fable.Core.JS

    type GenerateKeyParams = { name: string; length: int }
    type EncryptParams = { name: string; iv: Uint8Array }
    type CryptoKey = CryptoKey

    type Subtle =
        abstract member generateKey: GenerateKeyParams -> bool -> string [] -> CryptoKey Promise
        abstract member importKey: string -> Uint8Array -> string -> bool -> string [] -> CryptoKey Promise
        abstract member encrypt: EncryptParams -> CryptoKey -> Uint8Array -> ArrayBuffer Promise
        abstract member decrypt: EncryptParams -> CryptoKey -> ArrayBuffer -> ArrayBuffer Promise
        abstract member digest: string -> Uint8Array -> Uint8Array Promise

    type Crypto = { subtle: Subtle }

    [<Global("crypto")>]
    let crypto: Crypto = jsNative

open Fable.Core
open Fable.Core.JS
open System.Text

let private encrypt (key: string) (value: byte []) =
    async {
        let! rawKey =
            Encoding.UTF8.GetBytes key
            |> Constructors.Uint8Array.Create
            |> Crypto.crypto.subtle.digest "SHA-256"
            |> Async.AwaitPromise

        let rawKey = rawKey.slice (0, 16)

        let! key =
            Crypto.crypto.subtle.importKey "raw" rawKey "AES-CBC" true [| "encrypt"; "decrypt" |]
            |> Async.AwaitPromise

        let data = Constructors.Uint8Array.Create(value)

        let iv = Constructors.Uint8Array.Create(16)

        let! encrypted =
            Crypto.crypto.subtle.encrypt { name = "AES-CBC"; iv = iv } key data
            |> Async.AwaitPromise

        let encrypted = Constructors.Uint8Array.Create(encrypted)
        return encrypted
    }

let private decrypt (key: string) (data: ArrayBuffer) =
    async {
        let! rawKey =
            Encoding.UTF8.GetBytes key
            |> Constructors.Uint8Array.Create
            |> Crypto.crypto.subtle.digest "SHA-256"
            |> Async.AwaitPromise

        let rawKey = rawKey.slice (0, 16)

        let! key =
            Crypto.crypto.subtle.importKey "raw" rawKey "AES-CBC" true [| "encrypt"; "decrypt" |]
            |> Async.AwaitPromise

        let iv = Constructors.Uint8Array.Create(16)

        let! decrypted =
            Crypto.crypto.subtle.decrypt { name = "AES-CBC"; iv = iv } key data
            |> Async.AwaitPromise

        let decrypted = Constructors.Uint8Array.Create(decrypted)
        return decrypted
    }

open Fetch

let loadAllPayloads (server: string) (pass: string) : byte [] list Async =
    async {
        let! response =
            fetch server [ Method HttpMethod.GET ]
            |> Async.AwaitPromise

        let! buf = response.arrayBuffer () |> Async.AwaitPromise
        let! result = decrypt pass buf

        let result = result :> obj :?> byte []

        return [ result ]
    }

let upload (server: string) (pass: string) (data: byte []) : unit Async =
    async {
        let! payload = encrypt pass data
        let blob = Browser.Blob.Blob.Create([| payload |])

        let! _ =
            fetch
                server
                [ Method HttpMethod.POST
                  Body(BodyInit.Case1 blob) ]
            |> Async.AwaitPromise

        return ()
    }
