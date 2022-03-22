module Utils

// open Robertluo
// open System.IO

// let getFilename (path: string) = Path.GetFileName path

// let (|Found|_|) key map =
//     map
//     |> Map.tryFind key
//     |> Option.map (fun x -> x, Map.remove key map)

// let parserConfig path =
//     System.IO.File.ReadAllText path
//     |> Edn.parse
//     |> function
//         | FParsec.CharParsers.Success (edn, _, _) ->
//             match edn with
//             | EMap (Found (Edn.Kw (null, "server")) (EString x, _)) ->
//                 match edn with
//                 | EMap (Found (Edn.Kw (null, "directories")) (EVector dirs, _)) ->
//                     let dirs =
//                         dirs
//                         |> Array.choose
//                             (function
//                             | EString x -> Some x
//                             | _ -> None)

//                     Ok
//                         { server = x
//                           key = failwith "???"
//                           directories = dirs |> List.ofSeq }
//                 | e -> Error $"${e}"
//             | e -> Error $"${e}"
//         | e -> failwithf "%O" e
