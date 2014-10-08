module FsFirebaseTest.Tests.FirebaseUrl

open Xunit
open FsUnit.Xunit
open FsFirebase

let [<Fact>] ``Ordinary URI should not cause any exception``() =
    let uri = FirebaseUrl("http://examples.com")
    in (string uri) |> should equal "http://examples.com"

let [<Fact>] ``URI with authentication token``() =
    let uri = FirebaseUrl("http://examples.com", "ABCDEF")
    in (string uri) |> should equal "http://examples.com/?auth=ABCDEF"

let [<Fact>] ``Pretty print request``() =
    let uri = FirebaseUrl("http://examples.com", pretty=true)
    in (string uri) |> should equal "http://examples.com/?print=pretty"

let [<Fact>] ``URI with authentication token and pretty print`` () =
    let uri = FirebaseUrl("http://examples.com", "A12345", true)
    in (string uri) |> should equal "http://examples.com/?print=pretty&auth=A12345"