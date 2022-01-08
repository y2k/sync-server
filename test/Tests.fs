module Tests

open System
open Xunit
open Swensen.Unquote

let now = DateTime.Now

[<Fact>]
let ``test`` () = test <@ 2 * 2 <> 4 @>
