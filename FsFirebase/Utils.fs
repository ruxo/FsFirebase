﻿module FsFirebaseUtils

open System
open System.Threading

type MutableList<'item when 'item:equality>(init) =
    let mutable items: 'item list = init

    member x.Value = items

    member x.Update updater =
        let current = items
        let newItems = updater current
        if not <| obj.ReferenceEquals(current, Interlocked.CompareExchange(&items, newItems, current))
            then x.Update updater
            else x

    member x.Add item = x.Update (fun L -> item::L)
    member x.Remove item = x.Update (fun L -> List.filter (fun i -> i <> item) L)

    static member empty = new MutableList<'item>([])
    static member add item (l:MutableList<'item>) = l.Add item
    static member get (l:MutableList<'item>) = l.Value
    static member remove item (l:MutableList<'item>) = l.Remove item

type ObservableBase<'a>() =
    let subscriptionList = MutableList<IObserver<'a>>.empty        
    interface IObservable<'a> with
        member x.Subscribe observer = subscriptionList.Add observer |> ignore
                                      { new IDisposable with member x.Dispose() = subscriptionList.Remove observer |> ignore }

    member x.Complete() = subscriptionList.Value |> List.iter (fun obs -> obs.OnCompleted())
    member x.Push data = subscriptionList.Value |> List.iter (fun observer -> observer.OnNext data)
    member x.Error exn = subscriptionList.Value |> List.iter (fun obs -> obs.OnError exn)
