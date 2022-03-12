module App

module Application =
    type ItemType =
        | WatchLate
        | Url

    type State =
        { title: string
          url: string
          itemType: ItemType }

    type Msg =
        | UrlChanged of string
        | TitleChanged of string
        | ItemTypeChanged of ItemType
        | Add

    let update state msg = state

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
        abstract member digest: string -> Uint8Array -> Uint8Array Promise

    type Crypto = { subtle: Subtle }

    [<Global("crypto")>]
    let crypto: Crypto = jsNative

open Browser.Dom
open Fable.Core
open Fable.Core.JS
open System.Text

let foo () =
    async {
        let bytes =
            Encoding.UTF8.GetBytes("test")
            |> Constructors.Uint8Array.Create

        printfn "key bytes = %A" bytes

        let! rawKey =
            Crypto.crypto.subtle.digest "SHA-256" bytes
            |> Async.AwaitPromise

        let rawKey = rawKey.slice (0, 16)
        printfn "rawKey = %A" (Constructors.Uint8Array.Create rawKey)

        let! key =
            Crypto.crypto.subtle.importKey "raw" rawKey "AES-CBC" true [| "encrypt"; "decrypt" |]
            |> Async.AwaitPromise

        let data = Constructors.Uint8Array.Create(16)

        for i in 0..15 do
            data.[i] <- byte i

        printfn "data = %A" data

        let iv = Constructors.Uint8Array.Create(16)

        let! encrypted =
            Crypto.crypto.subtle.encrypt { name = "AES-CBC"; iv = iv } key data
            |> Async.AwaitPromise

        let encrypted = Constructors.Uint8Array.Create(encrypted)

        printfn "encrypted: (%O) %A" (encrypted.length) encrypted
    }
    |> Async.StartImmediate

open Fable.React
open Fable.React.Props

let view =
    FunctionComponent.Of<unit> (fun props ->
        form [ Class "form" ] [
            input [ Class "input"
                    Placeholder "URL"
                    Name "url" ]
            textarea [ Class "textarea"
                       Placeholder "Title"
                       Name "title" ] []
            div [ Class "field" ] [
                label [ Class "label" ] [
                    str "Link type"
                ]
                div [ Class "control" ] [
                    div [ Class "select" ] [
                        select [ Name "type" ] [
                            option [ Value "watch_late" ] [
                                str "Watch late (youtube)"
                            ]
                            option [ Value "url" ] [ str "URL" ]
                        ]
                    ]
                ]
            ]
            button [ Class "button" ] [ str "Add" ]
        ])

ReactDom.render (view (), (document.getElementById "root"))
