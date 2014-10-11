module FsFirebaseUtils

open System
open System.Threading

type MutableList<'item when 'item:equality>(init) =
    let mutable items: 'item list = init

    member x.Value = items

    member x.Update updater =
        let current = items
        let newItems = updater current
        if not <| obj.ReferenceEquals(current, Interlocked.CompareExchange(&items, newItems, current))
            then x.Update updater
            else x

    member x.Add item = x.Update (fun L -> item::L)
    member x.Remove item = x.Update (fun L -> List.filter (fun i -> i <> item) L)

    static member empty = new MutableList<'item>([])
    static member add item (l:MutableList<'item>) = l.Add item
    static member get (l:MutableList<'item>) = l.Value
    static member remove item (l:MutableList<'item>) = l.Remove item

type ObservableSource<'a>() =
    let subscriptionList = MutableList<IObserver<'a>>.empty        
    interface IObservable<'a> with
        member x.Subscribe observer = subscriptionList.Add observer |> ignore
                                      { new IDisposable with member x.Dispose() = subscriptionList.Remove observer |> ignore }

    member x.Complete() = subscriptionList.Value |> List.iter (fun obs -> obs.OnCompleted())
    member x.Push data = subscriptionList.Value |> List.iter (fun observer -> observer.OnNext data)
    member x.Error exn = subscriptionList.Value |> List.iter (fun obs -> obs.OnError exn)

    static member create() = new ObservableSource<'a>()
    static member get (source:ObservableSource<'a>) = source :> IObservable<'a>

module Observable =
    open System.Text

    let observeStream asyncRun (eventStream:ObservableSource<byte>) (stream:IO.Stream) =
        let buffer = Array.zeroCreate 16384
        let rec readStreamAsync() =
            async {
                let! read = stream.AsyncRead buffer
                if read > 0
                    then buffer
                         |> Seq.take read
                         |> Seq.iter (fun v -> eventStream.Push v)
                         asyncRun <| readStreamAsync()
                    else eventStream.Complete()
            }
        asyncRun <| readStreamAsync()

    let byteToCharStream (encoder:Encoding) stream =
        let buffer = [|0uy|]
        let charbuf = [| '\000' |]
        let decoder = encoder.GetDecoder()

        let decode v =
            buffer.[0] <- v
            decoder.GetChars(buffer, 0, 1, charbuf, 0)

        stream
        |> Observable.filter (fun v -> (decode v) = 1)
        |> Observable.map (fun _ -> charbuf.[0])

    let charToLineStream (stream:IObservable<char>) =
        let lineStream = ObservableSource.create()
        let sb = StringBuilder()

        let pushCurrent() =
            lineStream.Push (string sb)
            sb.Length <- 0
        let append c = ignore <| sb.Append(c:char)

        let pairStream = stream |> Observable.scan (fun (_, last0) c -> last0, c) ('\000', '\000')
        pairStream.Subscribe { new IObserver<char * char> with
                                   member x.OnNext pair =
                                      match pair with
                                      | _, '\n' -> pushCurrent()
                                      | '\r', c -> pushCurrent()
                                                   append c
                                      | _, c -> append c

                                   member x.OnCompleted() =
                                      if sb.Length > 0 then lineStream.Push (string sb)
                                      lineStream.Complete()
                                   member x.OnError exn = lineStream.Error exn
                             } |> ignore
        lineStream :> IObservable<string>


