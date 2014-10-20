#r "System.Net.Http.dll"
#r "Newtonsoft.json.dll"

#load "Utils.fs"
#load "ServerEventSource.fs"
#load "FsFirebase.fs"

open FsFirebase.Core
open ServerEventSource

let run title fasync =
    async {
        printfn "%s" title
        let! (result: FirebaseResult) = fasync
        match result with
        | Choice1Of2 _ -> ()
        | Choice2Of2 (code, msg) -> printfn "Put failed with %A: %s" code msg
    } |> Async.RunSynchronously

let sample =
    Json.fromKeyPairs
        [ "001", Json.jObject [ "name", JString "Alan Turing"
                                "birthday", JString "Jun 23, 1912"
                                "time", JObject None
                              ]
        ]

let runSample() =
    let eventStream = EventSourceProcessor()
    eventStream
    |> Observable.subscribe (function
                             | Comment comments -> printfn "COMMENT %A" comments
                             | ServerEvent (event, data) -> printfn "EVENT %s : %A" event data)
    |> ignore

    eventStream.Error
    |> Observable.subscribe (fun (code, msg) -> printfn "ERROR %A: %s" code msg)
    |> ignore

    eventStream.Start (FirebaseUrl("https://a7knbwy6th8.firebaseio-demo.com/.json")).Uri

    run "putAsync" <| putAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users.json") sample

    run "patchAsync" <| patchAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users/001.json")
                                   (Json.fromKeyPairs ["author", JString "RZ"; "temp", JString "dummy"])
    run "postAsync" <| postAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users/001/quotes.json") "\"A nice quote!\""
    run "postAsync" <| postAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users/001/quotes.json") "\"Another Quote!\""

    run "getAsync 001/quotes.json" <| getAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users/001/quotes.json")

    run "delete" <| deleteAsync (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users/001/temp.json")

    run "test auth should fail" <| getAsync (FirebaseUrl("https://a7knbwy6th8.firebaseio-demo.com/users/001/quotes.json", "DUMMY"))

    run "put timestamp" <| putTimestamp (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/users/001/time.json")

    printfn "Press ENTER to end..."
    System.Console.ReadLine() |> ignore

