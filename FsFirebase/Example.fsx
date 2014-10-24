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

let runSample (baseUrl:FirebaseUrl) =
    let eventStream = EventSourceProcessor()
    eventStream
    |> Observable.subscribe (function
                             | Comment comments -> printfn "COMMENT %A" comments
                             | ServerEvent (event, data) -> printfn "EVENT %s : %A" event data)
    |> ignore

    eventStream.Error
    |> Observable.subscribe (fun (code, msg) -> printfn "ERROR %A: %s" code msg)
    |> ignore

    eventStream.Start baseUrl.Uri

    run "putAsync" <| putAsync (baseUrl.Path "users") sample

    run "patchAsync" <| patchAsync (baseUrl.Path "users/001")
                                   (Json.fromKeyPairs ["author", JString "RZ"; "temp", JString "dummy"])
    run "postAsync" <| postAsync (baseUrl.Path "users/001/quotes") "\"A nice quote!\""
    run "postAsync" <| postAsync (baseUrl.Path "users/001/quotes") "\"Another Quote!\""

    run "getAsync 001/quotes.json" <| getAsync (baseUrl.Path "users/001/quotes")

    run "delete" <| deleteAsync (baseUrl.Path "users/001/temp")

    run "test auth should fail" <| getAsync (baseUrl |> FirebaseUrl.path "users/001/quotes" |> FirebaseUrl.changeAuth "DUMMY")

    run "put timestamp" <| putTimestamp (baseUrl.Path "users/001/time")

    printfn "Press ENTER to end..."
    System.Console.ReadLine() |> ignore

runSample (FirebaseUrl "https://a7knbwy6th8.firebaseio-demo.com/")