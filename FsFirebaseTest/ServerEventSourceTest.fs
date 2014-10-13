module ServerEventSourceTest

open System.Collections.Generic
open NUnit.Framework
open FsUnit
open FsFirebaseUtils
open ServerEventSource

let [<Test>] ``Check line interpretation algorithm``() =
    ServerEventSource._interpretLine "" |> should equal BlankLine
    ServerEventSource._interpretLine "event type" |> should equal (Field ("event type", ""))
    ServerEventSource._interpretLine " : Comment!" |> should equal (EventSourceType.Comment "Comment!")
    ServerEventSource._interpretLine "field: data" |> should equal (Field ("field", "data"))

let [<Test>] ``Fill network stream with an event 'greet' and data 'hello world!'``() =
    let output = ObservableSource.create()
    let ns = ServerEventSource._createNetworkStream(output, (fun _ -> ()), (fun _ -> ()))
    let buffer = Queue()
    output |> Observable.subscribe (fun e -> buffer.Enqueue e) |> ignore

    let data = System.Text.Encoding.UTF8.GetBytes("""
data: YHOO
data: +2
data: 10

"""                                              )
    data |> Array.iter (fun b -> ns.Push b) 

    let result = Seq.toList buffer

    result |> should equal [ ServerEvent ("", ["YHOO"; "+2"; "10"]) ]
