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

let testNetworkStream data expected =
    let output = ObservableSource.create()
    let ns = ServerEventSource._createNetworkStream(output, (fun _ -> ()), (fun _ -> ()))
    let buffer = Queue()
    output |> Observable.subscribe (fun e -> buffer.Enqueue e) |> ignore

    let data' = System.Text.Encoding.UTF8.GetBytes(data:string)
    data' |> Array.iter (fun b -> ns.Push b) 

    let result = Seq.toList buffer

    printfn "result = %A" result
    result |> should equal expected

(* These following test data are from the example of Event Stream via http://www.w3.org/TR/eventsource/#concept-event-stream-reconnection-time
*)
let [<Test>] ``Fill network stream with pattern 1``() =
    testNetworkStream 
        """
data: YHOO
data: +2
data: 10

"""
        [ ServerEvent ("", ["YHOO"; "+2"; "10"]) ]

let [<Test>] ``Network stream #2`` () =
    testNetworkStream
        """: test stream

data: first event
id: 1

data:second event
id

data:   third event"""
        [ Comment ["test stream"]
          ServerEvent ("", ["first event"])
          ServerEvent ("", ["second event"])
        ]

let [<Test>] ``Network stream #3`` () =
    testNetworkStream
        """data

data
data

data:"""
        [ ServerEvent ("", [""])
          ServerEvent ("", [""; ""])
        ]

let [<Test>] ``Network stream #4: two identical events`` () =
    testNetworkStream
        """data:test

data: test

"""
        [ ServerEvent ("", ["test"])
          ServerEvent ("", ["test"])
        ]

(* Test Firebase events *)
let [<Test>] ``Put Event`` () =
    testNetworkStream """event: put
                         data: {"path": "/", "data": {"a": 1, "b": 2}}

                         """
                      [ ServerEvent ("put", ["""{"path": "/", "data": {"a": 1, "b": 2}}"""]) ]