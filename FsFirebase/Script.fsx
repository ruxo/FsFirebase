#r "System.Net.Http.dll"
#r "Newtonsoft.json.dll"

#load "FsFirebase.fs"
open FsFirebase

printfn "Start!!"

let url = "https://a7knbwy6th8.firebaseio-demo.com/user.json"
let data = """{ "someone": { "name": "Alan Turing", "birthday": "June 23, 1912" } }"""
async {
    let! result = putAsync(url, data)
    match result with
    | Choice1Of2 r -> printfn "Response = %s" r
    | Choice2Of2 (code, msg) -> printfn "Put failed with %d: %s" code msg
} |> Async.RunSynchronously
