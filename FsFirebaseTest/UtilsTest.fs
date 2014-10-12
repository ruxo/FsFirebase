module UtilsTest

open System
open Xunit
open FsUnit.Xunit
open FsFirebaseUtils

let [<Fact>] ``Add item to a list of integer``() =
    MutableList.empty 
    |> MutableList.add 1
    |> MutableList.get
    |> should equal [1]

let [<Fact>] ``Add item to a list of few integers``() =
    (MutableList [1;2;3;4])
    |> MutableList.add 5
    |> MutableList.get
    |> should equal [5;1;2;3;4]

let [<Fact>] ``Remove a middle item from a list of integers``() =
    (MutableList [1;2;3;4;5])
    |> MutableList.remove 3
    |> MutableList.get
    |> should equal [1;2;4;5]

module Observable =
    let [<Fact>] ``Make stream an observable``()=
        let memStream = new System.IO.MemoryStream([| 1uy; 2uy; 3uy; 4uy; 5uy |])

        let d: byte list ref = ref []
        let source = ObservableSource.create()
        ObservableSource.get source
        |> Observable.subscribe (fun v -> d := v::(!d))
        |> ignore
    
        Observable.observeStream (Async.RunSynchronously) source memStream

        !d |> should equal [5uy; 4uy; 3uy; 2uy; 1uy]


    let [<Fact>] ``Observable library hooks source`` () =
        let singleObserver:IObserver<char> ref = ref null
        let source = { new IObservable<char> with
                         member x.Subscribe obs = singleObserver := obs
                                                  { new IDisposable with member o.Dispose() = ()}
                     }

        let charToInt = source
                      |> Observable.map (fun c -> int c)
        charToInt.Subscribe { new IObserver<int> with
                                member x.OnNext i = ()
                                member x.OnCompleted() = ()
                                member x.OnError exn = ()
                            } |> ignore

        (!singleObserver) |> should not' (be Null)
        
    let [<Fact>] ``Observable chains OnComplete()`` () =
        let singleObserver:IObserver<char> ref = ref null
        let source = { new IObservable<char> with
                         member x.Subscribe obs = singleObserver := obs
                                                  { new IDisposable with member o.Dispose() = ()}
                     }

        let charToInt = source
                      |> Observable.map (fun c -> int c)

        let completed = ref false
        charToInt.Subscribe { new IObserver<int> with
                                member x.OnNext i = printfn "%d" i
                                member x.OnCompleted() = completed := true
                                member x.OnError exn = ()
                            } |> ignore
        (!singleObserver).OnCompleted()

        (!completed) |> should be True

    open System.Text
    let [<Fact>] ``byteToCharStream can convert UTF8 string correctly``() =
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

    let [<Fact>] ``charToLineStream can split lines correctly`` () =
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