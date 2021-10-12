module RestMessageBrocker

open System.Net.Http
open System.Text
open System.IO
open System.Security.Cryptography

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

let sendEvent (url : string) key bytes =
    async {
        let bytes = encrypt key bytes
        use client = new HttpClient()
        do! client.PostAsync(url, new ByteArrayContent(bytes)) |> Async.AwaitTask |> Async.Ignore
    }

let private decrypt (key: string) (data: byte []) =
    use aes = makeAes key
    use inStream = new MemoryStream(data)
    use outStream = new MemoryStream()

    use cryptStream =
        new CryptoStream(inStream, aes.CreateDecryptor(aes.Key, aes.IV), CryptoStreamMode.Read)

    cryptStream.CopyTo(outStream)
    outStream.ToArray()

let private decodeEvent key (bytes: byte []) =
    decrypt key bytes

type Context = private { receivers : (byte [] -> unit) list }
     with static member empty = { receivers = [] }

let receiveUpdate (ctx: Context) key (url : string) (offset : int64) =
    async {
        use client = new HttpClient()
        let! response = client.GetAsync($"{url}/%i{offset}") |> Async.AwaitTask
        let newOffset = response.Headers.GetValues("x-message-offset") |> Seq.head |> int64
        let! bytes = response.Content.ReadAsByteArrayAsync() |> Async.AwaitTask
        let form = decodeEvent key bytes
        for f in ctx.receivers do
            f form
        return newOffset
    }

let registerDec (ctx : Context) (dec : byte [] -> 'a option) (f : 'a -> unit) =
    let g = fun data ->
        match dec data with
        | Some x ->
            f x
        | None -> ()
    { ctx with receivers = g :: ctx.receivers }
