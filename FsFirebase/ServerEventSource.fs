module ServerEventSource

open System
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open FsFirebaseUtils

type Milliseconds = Milliseconds of int

type EventSourceMessage =
    | Comment of string list
    | ServerEvent of string * string list

module InnerProcessor = 
    type EventSourceType =
        | BlankLine
        | Comment of string
        | Field of string * string

    let interpretLine (line:string) =
        let normalized = line.Trim()
        match normalized.IndexOf(':') with
        | -1 when String.IsNullOrEmpty(normalized) -> BlankLine
        | -1 -> Field (normalized, String.Empty)
        | 0 -> Comment <| normalized.Substring(1).TrimStart()
        | n -> Field (normalized.Substring(0,n).TrimEnd(), normalized.Substring(n+1).TrimStart())

    let createEventSourceStack  = Observable.byteToCharStream System.Text.Encoding.UTF8
                                   >> Observable.charToLineStream
                                   >> Observable.map interpretLine

    type ProcessorState =
        { Event:string
          Data: string list
          Comment: string list
        }
        static member empty = {Event=String.Empty; Data=[]; Comment=[]}

    let processServerEvent (stream:ObservableSource<EventSourceMessage>, updateLastId, updateReconnectionTime) current es =
        match es with
        | Field (field, data) ->
            match field with
            | "event" -> { current with Event=data }
            | "data" -> { current with Data=data::current.Data }
            | "id" -> updateLastId <| if String.IsNullOrEmpty data then None else Some data
                      current
            | "retry" -> match Int32.TryParse data with
                         | false, _ -> current
                         | true, value -> updateReconnectionTime <| Milliseconds value
                                          current
            | _ -> current

        | EventSourceType.Comment comment ->
            { current with Comment=comment::current.Comment }

        | BlankLine ->
            if current.Comment <> []
                then stream.Push <| EventSourceMessage.Comment (List.rev current.Comment)
                     { current with Comment=[] }
            elif current.Data <> []
                then stream.Push <| ServerEvent (current.Event, List.rev current.Data)
                     { current with Event=""; Data=[] }
                else current

    let createNetworkStream procParam =
        let networkSink = ObservableSource.create()
        networkSink
        |> createEventSourceStack
        |> Observable.scan (processServerEvent procParam) ProcessorState.empty
        |> Observable.subscribe (fun _ -> ())
        |> ignore
        networkSink

type RetryType = Temporary | Permanent
type ConnectionResult =
    | OK of IO.Stream
    | Failed of HttpStatusCode * string
    | Retry of Uri * RetryType
    | UseProxy of Uri
    | RetryLater

module InnerServer =
    [<Literal>]
    let TextEventStreamMime = "text/event-stream"

    let private __contentType (response:HttpResponseMessage) = response.Content.Headers.ContentType
    let private __location (response:HttpResponseMessage) = response.Headers.Location

    let createHttpClient proxy =
        match proxy with
        | Some uri -> let handler = new HttpClientHandler(UseProxy = true, Proxy = WebProxy(uri:Uri))
                      in new HttpClient(handler)
        | None -> new HttpClient()

    let connectEventSourceServer message (client:HttpClient) =
        async {
            let! response = client.SendAsync(message, HttpCompletionOption.ResponseHeadersRead) |> Async.AwaitTask
            match response.StatusCode with
            | HttpStatusCode.OK when (__contentType response).MediaType = TextEventStreamMime -> 
                let! responseStream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
                return OK responseStream
            | HttpStatusCode.OK ->
                return Failed (HttpStatusCode.NotImplemented, "Unrecognize response's content-type: " + (string <| __contentType response))
            | HttpStatusCode.UseProxy ->
                return UseProxy (__location response)
            | HttpStatusCode.MovedPermanently ->
                return Retry (__location response, Permanent)
            | HttpStatusCode.Found
            | HttpStatusCode.SeeOther
            | HttpStatusCode.TemporaryRedirect ->
                return Retry (__location response, Temporary)
            | HttpStatusCode.InternalServerError
            | HttpStatusCode.BadGateway
            | HttpStatusCode.ServiceUnavailable
            | HttpStatusCode.GatewayTimeout ->
                return RetryLater
            | _ ->
                return Failed (response.StatusCode, response.ReasonPhrase)
        }

    let createEventSourceMessage (uri:Uri) lastEventId = 
        let message = new HttpRequestMessage(HttpMethod.Get, string uri)
        message.Headers.Accept.Add (MediaTypeWithQualityHeaderValue TextEventStreamMime)
        message.Headers.CacheControl <- CacheControlHeaderValue(NoCache=true)
        if Option.isSome lastEventId
            then message.Headers.Add("Last-Event-ID", Option.get<string> lastEventId)
        message

type EventSourceProcessor(?initReconnectionTime, ?initLastId) =
    let stream = ObservableSource.create()
    let errorStream = ObservableSource.create()
    let mutable lastId: string option = initLastId
    let mutable reconnectionTime = defaultArg initReconnectionTime (Milliseconds 0)

    interface IObservable<EventSourceMessage> with
        member x.Subscribe observer = (stream :> IObservable<EventSourceMessage>).Subscribe observer

    member x.Error with get() = errorStream :> IObservable<HttpStatusCode * string>

    member x.Start(uri, ?proxy) =
        let message = InnerServer.createEventSourceMessage uri lastId
        async {
            let! response = InnerServer.createHttpClient proxy
                            |> InnerServer.connectEventSourceServer message
            match response with
            | OK networkStream ->
                let networkSink = InnerProcessor.createNetworkStream ( stream
                                                      , (fun id -> lastId <- id)
                                                      , (fun time -> reconnectionTime <- time)
                                                      )
                in Observable.observeStream (Async.RunSynchronously) networkSink networkStream
            | Failed (code, msg) -> errorStream.Push (code, msg)
            | Retry (newUri, _) -> x.Start newUri
            | UseProxy proxyUri -> x.Start(uri, proxyUri)
            | RetryLater -> let (Milliseconds t) = reconnectionTime
                            do! Async.Sleep t
                            x.Start uri
        }
        |> Async.Start
