module FsFirebaseTest

open NUnit.Framework
open FsUnit
open FsFirebase

[<Test>]
let ``When serializing a list of int into JSON`` () =
    let data = [1;2;3;4;5]

    let result = Json.fromObj data

    result |> should equal "[1,2,3,4,5]"