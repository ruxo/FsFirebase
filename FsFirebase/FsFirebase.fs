module FsFirebase.Core

open System
open System.Net.Http
open System.Net.Http.Headers
open Newtonsoft.Json
open FsFirebase.Utils

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

let (|EndsWith|_|) pattern (target:string) = if target.EndsWith( pattern, StringComparison.OrdinalIgnoreCase )
                                                 then Some target
                                                 else None

type FirebaseUrl(url, ?auth, ?pretty, ?shallow, ?priority) =
    static let JsonSignature = ".json"

    let auth' = defaultArg auth ""
    let pretty' = defaultArg pretty false
    let shallow' = defaultArg shallow false
    let priority' = defaultArg priority false

    let boolParameter flag v p = if flag then v::p else p

    let getAuth p = if auth' = "" then p else ("auth", auth')::p
    let getPretty = boolParameter pretty' ("print", "pretty")
    let getShallow = boolParameter shallow' ("shallow", "true")
    let getPriority = boolParameter priority' ("format", "export")

    static let jsonizeUrl (url:string) = match url.Trim() with
                                         | EndsWith JsonSignature trimed -> trimed
                                         | trimed -> VirtualPath.combine trimed JsonSignature
    let makeUri() = match (getAuth >> getPretty >> getShallow >> getPriority) [] with
                    | [] -> jsonizeUrl url
                    | pairs ->
                        let paramTexts = pairs
                                         |> List.map (fun (k,v) -> sprintf "%s=%s" (Uri.EscapeUriString(k)) (Uri.EscapeUriString(v)))
                        in sprintf "%s?%s" (jsonizeUrl url) (String.concat "&" paramTexts)
    
    member x.Uri = Uri(makeUri())
    member x.ChangeLocation (uri:Uri) = new FirebaseUrl(string uri, auth', pretty', shallow', priority')
    member x.Path path = new FirebaseUrl(VirtualPath.combine url path, auth', pretty', shallow', priority')
    member x.ChangeAuth newAuth = new FirebaseUrl(url, newAuth, pretty', shallow', priority')
    
    override x.ToString() = string x.Uri

    static member uri (url:FirebaseUrl) = url.Uri
    static member path path (url:FirebaseUrl) = url.Path path
    static member changeAuth newAuth (url:FirebaseUrl) = url.ChangeAuth newAuth

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

let private _requestAsync msg =
    async {
        use client = new HttpClient()

        let! (response :HttpResponseMessage) = Async.AwaitTask <| client.SendAsync msg
        let! content = Async.AwaitTask <| response.Content.ReadAsStringAsync()
        return match response.IsSuccessStatusCode with
               | true -> Choice1Of2 content
               | false -> Choice2Of2 (enum<FirebaseReturnCode>(int response.StatusCode), content)
    }

let private createRequestMessage (url:FirebaseUrl, m, data) =
    let msg = match data with
              | None -> new HttpRequestMessage(HttpMethod(m), string url)
              | Some d -> new HttpRequestMessage(HttpMethod(m), string url, Content=new StringContent(d))
    msg.Headers.Accept.Add( MediaTypeWithQualityHeaderValue("application/json") )
    msg

let getAsync (url:FirebaseUrl) = _requestAsync <| createRequestMessage( url, "GET", None)

let putAsync (url:FirebaseUrl) data = _requestAsync <| createRequestMessage (url, "PUT", Some data)

let putTimestamp url = putAsync url """{".sv":"timestamp"}"""

let patchAsync (url:FirebaseUrl) data = _requestAsync <| createRequestMessage (url, "PATCH", Some data)

let postAsync (url:FirebaseUrl) data = _requestAsync <| createRequestMessage (url, "POST", Some data)

let deleteAsync (url:FirebaseUrl) = _requestAsync <| createRequestMessage (url, "DELETE", None)

