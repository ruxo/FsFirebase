module FsFirebase

open System
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
        | JObject None -> null
        | JObject (Some pairs) -> new JObject( pairs
                                               |> List.map (fun (k,v) -> new JProperty(k, _jsonTokenize v) :> obj)
                                               |> List.toArray
                                             ) :> JToken
    let fromKeyPairs pairs = 
        let token = _jsonTokenize <| jObject pairs
        token.ToString(Formatting.None)

type FirebaseUrl(url, ?auth, ?pretty, ?shallow, ?priority) =
    let uri = Uri(url)
    let boolParameter flag v p = match defaultArg flag false with
                                 | false -> p
                                 | true -> v::p

    let getAuth p = match defaultArg auth "" with
                    | "" -> p
                    | txt -> ("auth", txt)::p
    let getPretty = boolParameter pretty ("print", "pretty")
    let getShallow = boolParameter shallow ("shallow", "true")
    let getPriority = boolParameter priority ("format", "export")

    override x.ToString() = match (getAuth >> getPretty >> getShallow >> getPriority) [] with
                            | [] -> url
                            | pairs ->
                                let paramTexts = pairs
                                                 |> List.map (fun (k,v) -> sprintf "%s=%s" (Uri.EscapeUriString(k)) (Uri.EscapeUriString(v)))
                                in sprintf "%s?%s" (string uri) (String.concat "&" paramTexts)

type FirebaseReturnCode =
    | OK = 200
    | NotFound = 404
    | BadRequest = 400
    | ExpectationFailed = 417
    | Forbidden = 403

type FirebaseResult = Choice<string, (FirebaseReturnCode * string)>

[<RequireQualifiedAccess>]
module FirebaseKeyMaker =
    let priority n = ".priority", JNumber n
    /// <summary>
    /// Set primitive value and its priority value.
    /// </summary>
    /// <remark>https://www.firebase.com/docs/rest/guide/retrieving-data.html</remark>
    let priorityPrimitive n v = Json.jObject [".value", v; priority n]

let private _requestAsync f =
    async {
        use client = new HttpClient()
        let! (response :HttpResponseMessage) = Async.AwaitTask <| f client
        let! content = Async.AwaitTask <| response.Content.ReadAsStringAsync()
        return match response.IsSuccessStatusCode with
               | true -> Choice1Of2 content
               | false -> Choice2Of2 (enum<FirebaseReturnCode>(int response.StatusCode), content)
    }

let getAsync (url:FirebaseUrl) = _requestAsync (fun client -> client.GetAsync(string url))

let putAsync (url:FirebaseUrl) data = _requestAsync (fun client -> client.PutAsync(string url, new StringContent(data)))

let putTimestamp url = putAsync url """{".sv":"timestamp"}"""

let patchAsync (url:FirebaseUrl) data =
    _requestAsync (fun client -> let msg = new HttpRequestMessage(new HttpMethod("PATCH"), string url, Content=new StringContent(data))
                                 in client.SendAsync(msg))

let postAsync (url:FirebaseUrl) data = _requestAsync (fun client -> client.PostAsync(string url, new StringContent(data)))

let deleteAsync (url:FirebaseUrl) = _requestAsync (fun client -> client.DeleteAsync(string url))