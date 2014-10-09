﻿module FsFirebaseUtils

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
