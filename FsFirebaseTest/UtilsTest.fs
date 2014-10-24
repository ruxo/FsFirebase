module UtilsTest

open System
open System.Collections.Generic
open System.Text
open FsUnit
open FsFirebase.Utils

module Observable =
    let subscribeObj observer (observable:IObservable<'a>) = observable.Subscribe observer
      
open Fuchu
[<Fuchu.Tests>]
let tests =
    testList "UtilsTests" [
        testList "Mutable List" [
            test "Add item to a list of integer" {
                MutableList.empty 
                |> MutableList.add 1
                |> MutableList.get
                |> should equal [1]
            }
            test "Add item to a list of few integers" {
                (MutableList [1..4])
                |> MutableList.add 5
                |> MutableList.get
                |> should equal [5;1;2;3;4]
            }
            test "Remove a middle item from a list of integers" {
                (MutableList [1..5])
                |> MutableList.remove 3
                |> MutableList.get
                |> should equal [1;2;4;5]
            }
        ]
        testList "Observable" [
            test "Make stream an observable" {
                let memStream = new System.IO.MemoryStream([| 1uy..5uy |])
                let d: byte list ref = ref []
                let source = ObservableSource.create()
                ObservableSource.get source
                |> Observable.subscribe (fun v -> d := v::(!d))
                |> ignore
    
                Observable.observeStream (Async.RunSynchronously) source memStream

                !d |> should equal [5uy; 4uy; 3uy; 2uy; 1uy]
            }
            test "Observable library hooks source" {
                let singleObserver:IObserver<char> ref = ref null
                in { new IObservable<char> with
                         member x.Subscribe obs = singleObserver := obs
                                                  { new IDisposable with member o.Dispose() = ()}
                   }
                   |> Observable.map (fun c -> int c)
                   |> Observable.subscribe (fun _ -> ())
                   |> ignore

                (!singleObserver) |> should not' (be Null)
            }
            test "Observable chains OnComplete()" {
                let singleObserver:IObserver<char> ref = ref null
                let completed = ref false

                { new IObservable<char> with
                    member x.Subscribe obs = singleObserver := obs
                                             { new IDisposable with member o.Dispose() = ()}
                }
                |> Observable.map (fun c -> int c)
                |> Observable.subscribeObj { new IObserver<int> with
                                               member x.OnNext i = printfn "%d" i
                                               member x.OnCompleted() = completed := true
                                               member x.OnError exn = ()
                                           }
                |> ignore

                (!singleObserver).OnCompleted()

                (!completed) |> should be True
            }
            test "byteToCharStream can convert UTF8 string correctly" {
                let expected = "สวัสดี\r\nชาวโลก!"
                let bytes = Encoding.UTF8.GetBytes(expected)
                let source = ObservableSource.create()

                let sb = Text.StringBuilder()
                source
                |> Observable.byteToCharStream Encoding.UTF8
                |> Observable.subscribe (fun c -> ignore <| sb.Append c)
                |> ignore

                bytes |> Array.iter (fun b -> source.Push b)

                (string sb) |> should equal expected
            }
            test "charToLineStream can split lines correctly" {
                let expected = "สวัสดี\rตื่นเถิด\r\nชาวโลก!\nsomeone said"
                let source = ObservableSource.create()

                let final: string list ref = ref []
                source
                |> Observable.charToLineStream
                |> Observable.scan (fun a line -> line::a) []
                |> Observable.subscribe (fun lines -> final := lines)
                |> ignore

                expected |> Seq.iter (fun c -> source.Push c)
                source.Complete()

                !final |> should equal ["someone said"; "ชาวโลก!"; "ตื่นเถิด"; "สวัสดี"]
            }
            testList "charToLineStream fire a blank line" [
                let pushBlankLine (data:string) =
                    let source = ObservableSource.create()
                    let final = Queue()

                    source
                    |> Observable.charToLineStream
                    |> Observable.subscribe (fun s -> final.Enqueue s)
                    |> ignore

                    Seq.iter source.Push data

                    final.Count |> should equal 1
                    final.Dequeue() |> should equal ""

                yield test "UNIX newline" { pushBlankLine "\n" }
                yield test "Windows newline" { pushBlankLine "\r\n" }
            ]
        ]
    ]