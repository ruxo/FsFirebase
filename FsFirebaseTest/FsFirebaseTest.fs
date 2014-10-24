module FsFirebaseTest.Tests.Json

open Fuchu
open FsUnit
open FsFirebase.Core

type SomeRecord = {key:string; value: obj}

[<Tests>]
let tests =
    testList "FsFirebase Core" [
        testCase "When serializing a list of int into JSON" <|
            fun _ -> Json.fromObj [1..5] |> should equal "[1,2,3,4,5]"

        testCase "Given a record, fromObj should return correct JSON" <|
            fun _ -> Json.fromObj {key="A"; value=99} |> should equal """{"key":"A","value":99}"""

        testCase "Given a list of key pairs, fromKeyPairs should return a correct JSON text" <|
            fun _ -> Json.fromKeyPairs [ "key", JString "A"
                                         "value", JNumber 99m
                                       ]
                     |> should equal """{"key":"A","value":99}"""

        testCase "Object with array value, the order should be preserved" <|
            fun _ -> Json.fromKeyPairs [ "key", JArray   [JString "A"; JString "B"; JString "C"]
                                         "value", JArray [JNumber  1m; JNumber  2m; JNumber  3m]
                                       ]
                     |> should equal """{"key":["A","B","C"],"value":[1,2,3]}"""

        testCase "Serialize an object with null" <|
            fun _ -> Json.fromKeyPairs ["somekey", JObject None] |> should equal """{"somekey":null}"""
    ]