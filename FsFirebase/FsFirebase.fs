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
    let auth' = defaultArg auth ""
    let pretty' = defaultArg pretty false
    let shallow' = defaultArg shallow false
    let priority' = defaultArg priority false

    let boolParameter flag v p = if flag then v::p else p

    let getAuth p = if auth' = "" then p else ("auth", auth')::p
    let getPretty = boolParameter pretty' ("print", "pretty")
    let getShallow = boolParameter shallow' ("shallow", "true")
    let getPriority = boolParameter priority' ("format", "export")

    let makeUri() = match (getAuth >> getPretty >> getShallow >> getPriority) [] with
                    | [] -> url
                    | pairs ->
                        let paramTexts = pairs
                                         |> List.map (fun (k,v) -> sprintf "%s=%s" (Uri.EscapeUriString(k)) (Uri.EscapeUriString(v)))
                        in sprintf "%s?%s" url (String.concat "&" paramTexts)
    
    member x.ChangeLocation (uri:Uri) = new FirebaseUrl(string uri, auth', pretty', shallow', priority')
    member x.Uri = Uri(makeUri())
    override x.ToString() = string x.Uri

    static member uri (url:FirebaseUrl) = url.Uri


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

module FirebaseStream =
    open System.Net
    open System.Net.Http.Headers
    open FsFirebaseUtils


    type EventMessage = { Event:string
                          Data:string
                          LastEvent:string
                          Retry:int
                        }
    type State =
        | Connecting
        | Open of EventMessage
        | Closed of (HttpStatusCode * string) option   // as an error message

    let createFrom (eventTracker:ObservableSource<State>) (url:FirebaseUrl) =
        let failConnection (status, reason) =
            eventTracker.Push <| Closed (Some (status, reason))
            eventTracker.Complete()

        eventTracker :> IObservable<State>
        (*
        eventSource.EventReceived
        |> Observable.map (fun arg -> { Event       =arg.Message.EventType
                                        Data        =arg.Message.Data
                                        LastEvent   =arg.Message.LastEventId
                                        Retry       =if arg.Message.Retry.HasValue
                                                         then arg.Message.Retry.Value
                                                         else 0
                                      })
                                      *)