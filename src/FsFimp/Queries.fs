module FsFimp.Queries

open System
open FsFimp.Fimp
open Thoth.Json.Net

module Discovery =

    let requestTopic = RequestTopic.create "pt:j1/mt:cmd/rt:discovery"
    let responseTopic = ResponseTopic.create "pt:j1/mt:evt/rt:discovery"

    let message =
        Message.create
            (Ctime.create DateTime.Now)
            (Props.create List.empty)
            Serv.System
            src
            (Type.create "cmd.discovery.request")
            (Uid.newUid())
            Val.Null

module Things =

    let requestTopic = RequestTopic.create "pt:j1/mt:cmd/rt:app/rn:vinculum/ad:1"
    let responseTopic = ResponseTopic.create "pt:j1/mt:rsp/rt:app/rn:vinculum/ad:things"

    let private createDefaultMessage =
        Message.create
            (Ctime.create DateTime.Now)
            (Props.create List.empty)
            Serv.Vinculum
            src
            (Type.create "cmd.pd7.request")
            (Uid.newUid())

    let createMessage value =
        value
        |> createDefaultMessage
        |> Message.withResponseTopic responseTopic

    let private encodeComponents components =
        Encode.object [
            "components",
            components
            |> List.map Component.encode
            |> Encode.list
        ]
        |> Some
        |> ObjectVal.create Cmd.Get None None
        |> Val.Object

    let listDevices =
        [
            Component.Device
        ]
        |> encodeComponents

    let listRooms =
        [
            Component.Room
        ]
        |> encodeComponents

    let listShortcuts =
        [
            Component.Shortcut
        ]
        |> encodeComponents

    let listDevicesAndRooms =
        [
            Component.Room
            Component.Device
        ]
        |> encodeComponents

    let listEverything =
        [
            Component.Thing
            Component.Device
            Component.Room
            Component.Mode
            Component.Shortcut
        ]
        |> encodeComponents

    let listDevice deviceId =
        ObjectVal.create Cmd.Get (Some Component.Device) (Some (ObjectRequestId.DeviceId deviceId)) None
        |> Val.Object
