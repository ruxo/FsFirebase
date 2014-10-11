module UtilsTest

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