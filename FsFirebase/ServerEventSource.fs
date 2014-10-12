module ServerEventSource
    open System
    open System.Net
    open System.Net.Http
    open System.Net.Http.Headers
    open FsFirebaseUtils

    [<Literal>]
    let TextEventStreamMime = "text/event-stream"

    type RetryType = Temporary | Permanent
    type ConnectionResult =
        | OK of IO.Stream
        | Failed of HttpStatusCode * string
        | Retry of Uri * RetryType
        | UseProxy of Uri
        | RetryLater

    let private __contentType (response:HttpResponseMessage) = response.Content.Headers.ContentType
    let private __location (response:HttpResponseMessage) = response.Headers.Location

    let createEventSourceMessage (uri:Uri) lastEventId = 
        let message = new HttpRequestMessage(HttpMethod.Get, string uri)
        message.Headers.Accept.Add (MediaTypeWithQualityHeaderValue TextEventStreamMime)
        message.Headers.CacheControl <- CacheControlHeaderValue(NoCache=true)
        if Option.isSome lastEventId
            then message.Headers.Add("Last-Event-ID", Option.get<string> lastEventId)
        message

    let connectEventSourceServer message =
        async {
            use client = new HttpClient()

            let! response = client.SendAsync(message) |> Async.AwaitTask
            match response.StatusCode with
            | HttpStatusCode.OK when (__contentType response).MediaType = TextEventStreamMime -> 
                let! responseStream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
                return OK responseStream
            | HttpStatusCode.OK ->
                return Failed (HttpStatusCode.OK, "Unrecognized content-type: " + (string <| __contentType response))
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

    type EventSourceType =
        | BlankLine
        | Comment of string
        | Field of string * string

    let private _interpretLine (line:string) =
        let normalized = line.Trim()
        match normalized.IndexOf(':') with
        | -1 when String.IsNullOrEmpty(normalized) -> BlankLine
        | -1 -> Field (normalized, String.Empty)
        | 0 -> Comment <| normalized.Substring(1)
        | n -> Field (normalized.Substring(1,n).TrimEnd(), normalized.Substring(n+1).TrimStart())

    type EventSourceMessage =
        | Comment of string list
        | ServerEvent of string * string list

    let private _createEventSourceStack =
        Observable.byteToCharStream System.Text.Encoding.UTF8
        >> Observable.charToLineStream
        >> Observable.map _interpretLine

    type ProcessorState = { Event:string
                            Data: string list
                            Comment: string list
                          }
    let createEventSource uri uriModifier =
        let stream = ObservableSource.create()
        let message = createEventSourceMessage uri None

        let processServerEvent current es =
            match es with
            | Field (field, data) ->
                match field with
                | "event" -> { current with Event=data }
                | "data" -> { current with Data=data::current.Data }
                | "id"
                | "retry" -> failwith "Not supported yet"
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

        let network = ObservableSource.create()
        network
        |> _createEventSourceStack
        |> Observable.scan processServerEvent {Event=""; Data=[]; Comment=[]}
        |> ignore

        async {
            let! response = connectEventSourceServer message
            match response with
            | OK networkStream ->
                Observable.observeStream (Async.RunSynchronously) network networkStream
            | _ -> failwith "not supported yet"
        } |> Async.Start
        stream :> IObservable<EventSourceMessage>