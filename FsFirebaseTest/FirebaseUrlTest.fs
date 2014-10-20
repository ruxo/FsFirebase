module FsFirebaseTest.Tests.FirebaseUrl

open NUnit.Framework
open FsUnit
open FsFirebase.Core

let [<Test>] ``Ordinary URI should not cause any exception``() =
    let uri = FirebaseUrl("http://examples.com")
    in (string uri) |> should equal "http://examples.com/"

let [<Test>] ``URI with authentication token``() =
    let uri = FirebaseUrl("http://examples.com", "ABCDEF")
    in (string uri) |> should equal "http://examples.com/?auth=ABCDEF"

let [<Test>] ``Pretty print request``() =
    let uri = FirebaseUrl("http://examples.com", pretty=true)
    in (string uri) |> should equal "http://examples.com/?print=pretty"

let [<Test>] ``URI with authentication token and pretty print`` () =
    let uri = FirebaseUrl("http://examples.com", "A12345", true)
    in (string uri) |> should equal "http://examples.com/?print=pretty&auth=A12345"

let [<Test>] ``Change location to http://google.com``() =
    let uri = FirebaseUrl("http://examples.com", "A12345", true)
    in string <| uri.ChangeLocation (System.Uri "http://google.com")
       |> should equal "http://google.com/?print=pretty&auth=A12345"
