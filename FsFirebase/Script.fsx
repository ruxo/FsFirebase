﻿#r "System.Net.Http.dll"
#r "Newtonsoft.json.dll"

#load "FsFirebase.fs"
open FsFirebase

let run title fasync =
    async {
        printfn "%s" title
        let! (result: FirebaseResult) = fasync
        match result with
        | Choice1Of2 r -> printfn "Response = %s" r
        | Choice2Of2 (code, msg) -> printfn "Put failed with %d: %s" code msg
    } |> Async.RunSynchronously

let sample =
    Json.fromKeyPairs
        [ "001", Json.jObject [ "name", JString "Alan Turing"
                                "birthday", JString "Jun 23, 1912"
                              ]
        ]
run "putAsync" <| putAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users.json") sample

run "patchAsync" <| patchAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users/001.json")
                               (Json.fromKeyPairs ["author", JString "RZ"; "temp", JString "dummy"])
run "postAsync" <| postAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users/001/quotes.json") "\"A nice quote!\""
run "postAsync" <| postAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users/001/quotes.json") "\"Another Quote!\""

run "getAsync 001/quotes.json" <| getAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users/001/quotes.json")

run "delete" <| deleteAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users/001/temp.json")

run "test auth should fail" <| getAsync (FirebaseUrl("https://a7knbwy6th8.firebaseio-demo.com/users/001/quotes.json", "DUMMY"))