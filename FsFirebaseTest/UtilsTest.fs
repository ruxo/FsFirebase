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