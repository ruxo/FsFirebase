module FsFirebaseTest.Tests.Json

open Xunit
open FsUnit.Xunit
open FsFirebase

let [<Fact>] ``When serializing a list of int into JSON`` () =
    Json.fromObj [1;2;3;4;5] |> should equal "[1,2,3,4,5]"

type SomeRecord = {key:string; value: obj}
let [<Fact>] ``Given a record, fromObj should return correct JSON`` () =
    Json.fromObj {key="A"; value=99} |> should equal """{"key":"A","value":99}"""

let [<Fact>] ``Given a list of key pairs, fromKeyPairs should return a correct JSON text`` () =
    Json.fromKeyPairs [ "key", JString "A"
                        "value", JNumber 99m
                      ]
    |> should equal """{"key":"A","value":99}"""

let [<Fact>] ``Object with array value, the order should be preserved`` () =
    Json.fromKeyPairs [ "key", JArray [JString "A"; JString "B"; JString "C"]
                        "value", JArray [JNumber 1m; JNumber 2m; JNumber 3m]
                      ]
    |> should equal """{"key":["A","B","C"],"value":[1,2,3]}"""

