module FsFirebase

open System.Net.Http
open Newtonsoft.Json

[<RequireQualifiedAccess>]
module Json =
    let fromObj o = JsonConvert.SerializeObject (o:obj)


let putAsync(url:string, data) =
    let json = new StringContent(data)

    async {
        use client = new HttpClient()
        let! response = Async.AwaitTask <| client.PutAsync(url, json)
        let! content = Async.AwaitTask <| response.Content.ReadAsStringAsync()
        return match response.IsSuccessStatusCode with
               | true -> Choice1Of2 content
               | false -> Choice2Of2 (int response.StatusCode, content)
    }
