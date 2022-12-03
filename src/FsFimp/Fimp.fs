module FsFimp.Fimp

open System
open Thoth.Json.Net

type RequestTopic = private RequestTopic of string
module RequestTopic =
    let create topic =
        RequestTopic topic

    let value (RequestTopic topic) = topic

type ResponseTopic = private ResponseTopic of string
module ResponseTopic =
    let create topic =
        ResponseTopic topic

    let createFromResourceType topic =
        create $"pt:j1/mt:evt{topic}"

    let value (ResponseTopic topic) = topic

[<RequireQualifiedAccess>]
type Cmd =
    | Get
    | Set
    | Delete
    | Edit

module Cmd =
    let encode (value: Cmd) : JsonValue =
        match value with
        | Cmd.Get -> Encode.string "get"
        | Cmd.Set -> Encode.string "set"
        | Cmd.Delete -> Encode.string "delete"
        | Cmd.Edit -> Encode.string "edit"

[<RequireQualifiedAccess>]
type Component =
    | Config
    | Device
    | House
    | Hub
    | Mode
    | Room
    | Service
    | Shortcut
    | Thing

module Component =
    let encode (value: Component) : JsonValue =
        match value with
        | Component.Config -> Encode.string "config"
        | Component.Device -> Encode.string "device"
        | Component.House -> Encode.string "house"
        | Component.Hub -> Encode.string "hub"
        | Component.Mode -> Encode.string "mode"
        | Component.Room -> Encode.string "room"
        | Component.Service -> Encode.string "service"
        | Component.Shortcut -> Encode.string "shortcut"
        | Component.Thing -> Encode.string "thing"

[<RequireQualifiedAccess>]
type Mode =
    | Home
    | Away
    | Sleep
    | Vacation

module Mode =
    let encode (value: Mode) : JsonValue =
        match value with
        | Mode.Home -> Encode.string "home"
        | Mode.Away -> Encode.string "away"
        | Mode.Sleep -> Encode.string "sleep"
        | Mode.Vacation -> Encode.string "vacation"

[<RequireQualifiedAccess>]
type ObjectRequestId =
    | DeviceId of int
    | Mode of Mode
    | FireAlarm of Enabled: bool * Supported: bool

module ObjectRequestId =
    let encode (value: ObjectRequestId) : JsonValue =
        match value with
        | ObjectRequestId.DeviceId v -> Encode.int v
        | ObjectRequestId.Mode m -> Mode.encode m
        | ObjectRequestId.FireAlarm (isEnabled, isSupported) ->
            Encode.object [
                "enabled", Encode.bool isEnabled
                "supported", Encode.bool isSupported
            ]

type ObjectVal =
    { Cmd: Cmd
      Component: Component option
      Id: ObjectRequestId option
      Param: JsonValue option }

module ObjectVal =
    let create cmd component' id param =
        { Cmd = cmd
          Component = component'
          Id = id
          Param = param }

    let encode (value: ObjectVal) =
        Encode.object [
            "cmd", Cmd.encode value.Cmd

            match value.Component with
            | Some c ->
                "component", Component.encode c
            | None ->
                "component", Encode.nil

            match value.Id with
            | Some id ->
                "id", ObjectRequestId.encode id
            | None ->
                "id", Encode.nil

            match value.Param with
            | Some p ->
                "param", p
            | None ->
                "param", Encode.nil
        ]

[<RequireQualifiedAccess>]
type Val =
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | Null
    | Str_array of string array
    | Int_array of int array
    | Float_array of float array
    | Int_map of Map<string, int>
    | Str_map of Map<string, string>
    | Float_map of Map<string, float>
    | Bool_map of Map<string, bool>
    | Object of ObjectVal
    | Base64 of string

module Val =

    let encode (value: Val) =
        match value with
        | Val.String x -> ["val_t", Encode.string "string"; "val", Encode.string x]
        | Val.Int x -> ["val_t", Encode.string "int"; "val", Encode.int x]
        | Val.Float x -> ["val_t", Encode.string "float"; "val", Encode.float x]
        | Val.Bool x -> ["val_t", Encode.string "bool"; "val", Encode.bool x]
        | Val.Null -> ["val_t", Encode.string "null"; "val", Encode.string "null"]
        | Val.Str_array xs -> ["val_t", Encode.string "string_array"; "val", xs |> Array.map Encode.string |> Encode.array]
        | Val.Int_array xs -> ["val_t", Encode.string "int_array"; "val", xs |> Array.map Encode.int |> Encode.array]
        | Val.Float_array xs -> ["val_t", Encode.string "float_array"; "val", xs |> Array.map Encode.float |> Encode.array]
        | Val.Int_map x -> ["val_t", Encode.string "int_map"; "val", x |> Map.map (fun _ v -> Encode.int v) |> Encode.dict]
        | Val.Str_map x -> ["val_t", Encode.string "str_map"; "val", x |> Map.map (fun _ v -> Encode.string v) |> Encode.dict]
        | Val.Float_map x -> ["val_t", Encode.string "float_map"; "val", x |> Map.map (fun _ v -> Encode.float v) |> Encode.dict]
        | Val.Bool_map x -> ["val_t", Encode.string "bool_map"; "val", x |> Map.map (fun _ v -> Encode.bool v) |> Encode.dict]
        | Val.Object x -> ["val_t", Encode.string "object"; "val", ObjectVal.encode x]
        | Val.Base64 x -> ["val_t", Encode.string "base64"; "val", Encode.string x]

type CorId = private CorId of CorrelationId: string
module CorId =
    let create correlationId =
        CorId correlationId

    let encode (CorId correlationId) =
        Encode.string correlationId

type Ctime = private Ctime of CreationTime: DateTime
module Ctime =
    let create creationTime =
        Ctime creationTime

    let encode (Ctime creationTime) =
        Encode.datetime creationTime

type Props = private Props of Properties: Map<string, string>
module Props =
    let create items =
        Map.ofSeq items
        |> Props

    let empty = Props Map.empty

    let encode (Props props) =
        props
        |> Map.map (fun _ -> Encode.string)
        |> Encode.dict

type RespTo = private RespTo of ResponseTopic: string
module RespTo =
    let create responseTopic =
        RespTo responseTopic

    let encode (RespTo responseTopic) =
        Encode.string responseTopic

[<RequireQualifiedAccess>]
type Serv =
    | Vinculum
    | System
    | Gateway
    | OutBinSwitch
    | OutLevelSwitch
    | ColorControl
module Serv =
    let encode (src: Serv) =
        match src with
        | Serv.Vinculum -> Encode.string "vinculum"
        | Serv.System -> Encode.string "system"
        | Serv.Gateway -> Encode.string "gateway"
        | Serv.OutBinSwitch -> Encode.string "out_bin_switch"
        | Serv.OutLevelSwitch -> Encode.string "out_lvl_switch"
        | Serv.ColorControl -> Encode.string "color_ctrl"

type Src = private Src of Source: string
module Src =
    let create source =
        Src source

    let encode (Src source) =
        Encode.string source

type TagsList = private TagsList of string list
module TagsList =
    let create tags =
        TagsList tags

    let encode (TagsList tags) =
        tags
        |> List.map Encode.string
        |> Encode.list

type Type = private Type of InterfaceType: string
module Type =
    let create interfaceType =
        Type interfaceType

    let encode (Type t) =
        Encode.string t

type Uid = private Uid of MessageIdentifier: Guid
module Uid =
    let create uid =
        Uid uid

    let newUid () =
        create (Guid.NewGuid())

    let encode (Uid uid) =
        Encode.guid uid

type Ver = private Ver of string
module Ver =
    let defaultVer = Ver "1"
    let encode (Ver ver) =
        Encode.string ver

type Message =
    { CorId: CorId option
      Ctime: Ctime
      Props: Props
      RespTo: RespTo option
      Serv: Serv
      Src: Src // Set only for commands
      Tags: TagsList option
      Type: Type
      Uid: Uid
      Val: Val
      Ver: Ver }

module Message =

    let create ctime props serv src interfaceType uid value =
        { CorId = None
          Ctime = ctime
          Props = props
          RespTo = None
          Serv = serv
          Src = src
          Tags = None
          Type = interfaceType
          Uid = uid
          Val = value
          Ver = Ver.defaultVer }

    let createTimeStamped props serv src interfaceType value =
        create
            (Ctime.create DateTime.Now)
            props
            serv
            src
            interfaceType
            (Uid.newUid())
            value

    let withCorrelationId corId msg =
        { msg with CorId = Some corId }

    let withResponseTopic (ResponseTopic topic) msg =
        { msg with RespTo = Some (RespTo.create topic) }

    let withTags tags msg =
        { msg with Tags = Some tags }

    let private encodeOpt (f: 'a -> JsonValue) (value: 'a option) =
        value
        |> Option.map f
        |> Option.defaultValue Encode.nil

    let encode (msg: Message) =
        Encode.object [
            "corid", msg.CorId |> encodeOpt CorId.encode
            "ctime", msg.Ctime |> Ctime.encode
            "props", msg.Props |> Props.encode
            "resp_to", msg.RespTo |> encodeOpt RespTo.encode
            "serv", msg.Serv |> Serv.encode
            "src", msg.Src |> Src.encode
            "tags", msg.Tags |> encodeOpt TagsList.encode
            "type", msg.Type |> Type.encode
            "uid", msg.Uid |> Uid.encode
            yield! msg.Val |> Val.encode
            "ver", msg.Ver |> Ver.encode
        ]

let app = "fsfimp"
let src = Src.create app
