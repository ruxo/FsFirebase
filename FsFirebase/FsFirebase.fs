module FsFirebase

open System.Net.Http
open Newtonsoft.Json

type JSON =
    | JString of string
    | JNumber of decimal
    | JArray of JSON list
    | JObject of (string * JSON) list option

[<RequireQualifiedAccess>]
module Json =
    open System
    open Newtonsoft.Json.Linq

    let private _jsonAdd(json:JObject, (key, value)) =
        json.[key] <- JToken.FromObject(value)
        json
    let fromObj o = JsonConvert.SerializeObject (o:obj)
    let jObject pairs = JSON.JObject (Some pairs)
    let rec private _jsonTokenize = function
        | JString s -> JToken.op_Implicit s
        | JNumber n -> if n = Decimal.Truncate n
                           then JToken.op_Implicit (int n)
                           else JToken.op_Implicit n
        | JArray array -> new JArray(array
                                     |> List.map (fun el -> _jsonTokenize el :> obj)
                                     |> List.toArray
                                    ) :> JToken
        | JObject None -> JToken.FromObject null
        | JObject (Some pairs) -> new JObject( pairs
                                               |> List.map (fun (k,v) -> new JProperty(k, _jsonTokenize v) :> obj)
                                               |> List.toArray
                                             ) :> JToken
    let fromKeyPairs pairs = 
        let token = _jsonTokenize <| jObject pairs
        token.ToString(Formatting.None)

type FirebaseResult = Choice<string, (int * string)>

let private _requestAsync f =
    async {
        use client = new HttpClient()
        let! (response :HttpResponseMessage) = Async.AwaitTask <| f client
        let! content = Async.AwaitTask <| response.Content.ReadAsStringAsync()
        return match response.IsSuccessStatusCode with
               | true -> Choice1Of2 content
               | false -> Choice2Of2 (int response.StatusCode, content)
    }

let getAsync (url:string) = _requestAsync (fun client -> client.GetAsync(url))

let putAsync (url:string) data = _requestAsync (fun client -> client.PutAsync(url, new StringContent(data)))

let patchAsync (url:string) data =
    _requestAsync (fun client -> let msg = new HttpRequestMessage(new HttpMethod("PATCH"), url, Content=new StringContent(data))
                                 in client.SendAsync(msg))

let postAsync (url:string) data = _requestAsync (fun client -> client.PostAsync(url, new StringContent(data)))

let deleteAsync (url:string) = _requestAsync (fun client -> client.DeleteAsync(url))