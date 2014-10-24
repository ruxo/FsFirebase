module ServerEventSourceTest

open System.Collections.Generic
open Fuchu
open FsUnit
open FsFirebase.Utils
open ServerEventSource
open ServerEventSource.InnerProcessor

let testNetworkStream data (expected:EventSourceMessage list) =
    let output = ObservableSource.create()
    let ns = createNetworkStream(output, (fun _ -> ()), (fun _ -> ()))
    let buffer = Queue()
    output |> Observable.subscribe (fun e -> buffer.Enqueue e) |> ignore

    let data' = System.Text.Encoding.UTF8.GetBytes(data:string)
    data' |> Array.iter (fun b -> ns.Push b) 

    let result = Seq.toList buffer

    result |> should equal expected

[<Tests>]
let tests =
    testList "Server Event Source Test" [
        testList "Line interpreter test" [
            testCase "Blank string is BlankLine" <|
                fun _ -> interpretLine "" |> should equal BlankLine
            testCase "'event type' is becoming the Event Type field" <|
                fun _ -> interpretLine "event type" |> should equal (Field ("event type", ""))
            testCase "Comment has Comment Type" <|
                fun _ -> interpretLine " : Comment!" |> should equal (EventSourceType.Comment "Comment!")
            testCase "'field: data' format is put in Field properly" <|
                fun _ -> interpretLine "field: data" |> should equal (Field ("field", "data"))
        ]
        testList "Network stream test" [
            (*
             * These following test data are from the example of Event Stream via http://www.w3.org/TR/eventsource/#concept-event-stream-reconnection-time
             *)
            testCase "Fill network stream with pattern 1" <|
                fun _ -> testNetworkStream "data: YHOO\n\
                                            data: +2\n\
                                            data: 10\n\
                                            \n"
                         <| [ ServerEvent ("", ["YHOO"; "+2"; "10"]) ]

            testCase "Network stream #2" <|
                fun _ -> testNetworkStream ": test stream\n\
                                            \n\
                                            data: first event\n\
                                            id: 1\n\
                                            \n\
                                            data:second event\n\
                                            id\n\
                                            \n\
                                            data:   third event"
                         <| [ EventSourceMessage.Comment ["test stream"]
                              ServerEvent ("", ["first event"])
                              ServerEvent ("", ["second event"])
                            ]
                            
            testCase "Network stream #3" <|
                fun _ -> testNetworkStream "data\n\
                                            \n\
                                            data\n\
                                            data\n\
                                            \n\
                                            data:"
                         <| [ ServerEvent ("", [""])
                              ServerEvent ("", [""; ""])
                            ]

            testCase "Network stream #4: two identical events" <|
                fun _ -> testNetworkStream "data:test\n\
                                            \n\
                                            data: test\n\
                                            \n"
                         <| [ ServerEvent ("", ["test"])
                              ServerEvent ("", ["test"])
                            ]
        ]
        testList "Firebase Event test" [
            testCase "Put Event" <|
                fun _ -> testNetworkStream "event: put\n\
                                            data: {\"path\": \"/\", \"data\": {\"a\": 1, \"b\": 2}}\n\
                                            \n"
                         <| [ ServerEvent ("put", ["""{"path": "/", "data": {"a": 1, "b": 2}}"""]) ]
        ]
    ]