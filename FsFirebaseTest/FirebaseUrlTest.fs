module FsFirebaseTest.Tests.FirebaseUrl

open Fuchu
open FsUnit
open FsFirebase.Core

[<Tests>]
let tests =
    testList "Firebase URL test" [
        test "Ordinary URI should not cause any exception" {
            let uri = FirebaseUrl("http://examples.com")
            in (string uri) |> should equal "http://examples.com/.json"
        }
        test "URI with authentication token" {
            let uri = FirebaseUrl("http://examples.com", "ABCDEF")
            in (string uri) |> should equal "http://examples.com/.json?auth=ABCDEF"
        }
        test "Pretty print request" {
            let uri = FirebaseUrl("http://examples.com", pretty=true)
            in (string uri) |> should equal "http://examples.com/.json?print=pretty"
        }
        test "URI with authentication token and pretty print" {
            let uri = FirebaseUrl("http://examples.com", "A12345", true)
            in (string uri) |> should equal "http://examples.com/.json?print=pretty&auth=A12345"
        }
        test "Change location to http://google.com" {
            let uri = FirebaseUrl("http://examples.com", "A12345", true)
            let newloc = uri.ChangeLocation (System.Uri "http://google.com")
            in (string newloc) |> should equal "http://google.com/.json?print=pretty&auth=A12345"
        }
        test "Try specify resource" {
            let uri = FirebaseUrl "http://examples.com/chapter1"
            in (string uri) |> should equal "http://examples.com/chapter1/.json"
        }
    ]