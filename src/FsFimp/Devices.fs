module FsFimp.Devices

open Thoth.Json.Net

type Interface =
    // Command interfaces
    | CmdLevelGetReport
    | CmdLevelSet
    | CmdConfigGetReport
    | CmdConfigSet
    | CmdGroupAddMembers
    | CmdGroupDeleteMembers
    | CmdGroupGetMembers
    | CmdPingSend
    | CmdMeterGetReport
    | CmdMeterReset
    | CmdModeGetReport
    | CmdModeSet
    | CmdSetpointGetReport
    | CmdSetpointSet
    | CmdVersionGetReport
    | CmdBinarySet
    | CmdColorSet

    // Event interfaces
    | EvtLevelReport
    | EvtConfigReport
    | EvtGroupMembersReport
    | EvtPingReport
    | EvtMeterReport
    | EvtModeReport
    | EvtSetpointReport
    | EvtVersionReport

module Interface =
    let toString (value: Interface) : string =
        match value with
        // Commands
        | CmdLevelGetReport -> "cmd.lvl.get_report"
        | CmdLevelSet -> "cmd.lvl.set"
        | CmdConfigGetReport -> "cmd.config.get_report"
        | CmdConfigSet -> "cmd.config.set"
        | CmdGroupAddMembers -> "cmd.group.add_members"
        | CmdGroupDeleteMembers -> "cmd.group.delete_members"
        | CmdGroupGetMembers -> "cmd.group.get_members"
        | CmdPingSend -> "cmd.ping.send"
        | CmdMeterGetReport -> "cmd.meter.get_report"
        | CmdMeterReset -> "cmd.meter.reset"
        | CmdModeGetReport -> "cmd.mode.get_report"
        | CmdModeSet -> "cmd.mode.set"
        | CmdSetpointGetReport -> "cmd.setpoint.get_report"
        | CmdSetpointSet -> "cmd.setpoint.set"
        | CmdVersionGetReport -> "cmd.version.get_report"
        | CmdBinarySet -> "cmd.binary.set"
        | CmdColorSet -> "cmd.color.set"

        // Events
        | EvtLevelReport -> "evt.lvl.report"
        | EvtConfigReport -> "evt.config.report"
        | EvtGroupMembersReport -> "evt.group.members_report"
        | EvtPingReport -> "evt.ping.report"
        | EvtMeterReport -> "evt.meter.report"
        | EvtModeReport -> "evt.mode.report"
        | EvtSetpointReport -> "evt.setpoint.report"
        | EvtVersionReport -> "evt.version.report"

    let fromString (str: string) : Interface option =
        match str with
        // Commands
        | "cmd.lvl.get_report" -> Some CmdLevelGetReport
        | "cmd.lvl.set" -> Some CmdLevelSet
        | "cmd.config.get_report" -> Some CmdConfigGetReport
        | "cmd.config.set" -> Some CmdConfigSet
        | "cmd.group.add_members" -> Some CmdGroupAddMembers
        | "cmd.group.delete_members" -> Some CmdGroupDeleteMembers
        | "cmd.group.get_members" -> Some CmdGroupGetMembers
        | "cmd.ping.send" -> Some CmdPingSend
        | "cmd.meter.get_report" -> Some CmdMeterGetReport
        | "cmd.meter.reset" -> Some CmdMeterReset
        | "cmd.mode.get_report" -> Some CmdModeGetReport
        | "cmd.mode.set" -> Some CmdModeSet
        | "cmd.setpoint.get_report" -> Some CmdSetpointGetReport
        | "cmd.setpoint.set" -> Some CmdSetpointSet
        | "cmd.version.get_report" -> Some CmdVersionGetReport
        | "cmd.binary.set" -> Some CmdBinarySet
        | "cmd.color.set" -> Some CmdColorSet

        // Events
        | "evt.lvl.report" -> Some EvtLevelReport
        | "evt.config.report" -> Some EvtConfigReport
        | "evt.group.members_report" -> Some EvtGroupMembersReport
        | "evt.ping.report" -> Some EvtPingReport
        | "evt.meter.report" -> Some EvtMeterReport
        | "evt.mode.report" -> Some EvtModeReport
        | "evt.setpoint.report" -> Some EvtSetpointReport
        | "evt.version.report" -> Some EvtVersionReport
        | _ -> None

    let decoder: Decoder<Interface list> =
        Decode.list Decode.string
        |> Decode.map (List.choose fromString)

type ServiceInfo =
    { Address: string
      Interfaces: Interface list
      Props: Map<string, JsonValue> }

// And update the decoder accordingly
module ServiceInfo =
    let decoder : Decoder<ServiceInfo> =
        Decode.object (fun get ->
            { Address = get.Required.Field "addr" Decode.string
              Interfaces = get.Required.Field "intf" Interface.decoder
              Props = get.Required.Field "props" (Decode.dict Decode.value) })

type Service =
    | Battery of ServiceInfo
    | BurglarAlarm of ServiceInfo
    | Luminance of ServiceInfo
    | Presence of ServiceInfo
    | Temperature of ServiceInfo
    | Meter of ServiceInfo
    | Thermostat of ServiceInfo
    | Color of ServiceInfo
    | Dimmer of ServiceInfo
    | Scene of ServiceInfo
    | Switch of ServiceInfo
    | Basic of ServiceInfo

module Service =
    let decoder : Decoder<Service list> =
        Decode.object (fun get ->
            [
                get.Optional.Field "battery" ServiceInfo.decoder |> Option.map Battery
                get.Optional.Field "alarm_burglar" ServiceInfo.decoder |> Option.map BurglarAlarm
                get.Optional.Field "sensor_lumin" ServiceInfo.decoder |> Option.map Luminance
                get.Optional.Field "sensor_presence" ServiceInfo.decoder |> Option.map Presence
                get.Optional.Field "sensor_temp" ServiceInfo.decoder |> Option.map Temperature
                get.Optional.Field "meter_elec" ServiceInfo.decoder |> Option.map Meter
                get.Optional.Field "thermostat" ServiceInfo.decoder |> Option.map Thermostat
                get.Optional.Field "color_ctrl" ServiceInfo.decoder |> Option.map Color
                get.Optional.Field "out_lvl_switch" ServiceInfo.decoder |> Option.map Dimmer
                get.Optional.Field "scene_ctrl" ServiceInfo.decoder |> Option.map Scene
                get.Optional.Field "out_bin_switch" ServiceInfo.decoder |> Option.map Switch
                get.Optional.Field "basic" ServiceInfo.decoder |> Option.map Basic
            ]
            |> List.choose id)

type DeviceParams = Map<string, JsonValue>

module DeviceParams =
    let decoder : Decoder<DeviceParams> = Decode.dict Decode.value

type DeviceKind =
    | Appliance
    | Thermostat
    | Light
    | Battery
    | Sensor
    | FireDetector
    | LeakDetector
    | Meter
    | Unknown of string

module DeviceKind =
    let decoder: Decoder<DeviceKind> =
        Decode.object (fun get ->
            let supported = get.Required.Field "supported" (Decode.dict (Decode.list Decode.string))
            let deviceType = get.Optional.Field "type" Decode.string

            match deviceType with
            | None ->
                if supported.ContainsKey "appliance" then Appliance
                elif supported.ContainsKey "thermostat" then Thermostat
                elif supported.ContainsKey "sensor" then Sensor
                elif supported.ContainsKey "light" then Light
                elif supported.ContainsKey "battery" then Battery
                elif supported.ContainsKey "fire_detector" then FireDetector
                elif supported.ContainsKey "leak_detector" then LeakDetector
                elif supported.ContainsKey "meter" then Meter
                else Unknown "Unknown"
            | Some "appliance" -> Appliance
            | Some "thermostat" -> Thermostat
            | Some "light" -> Light
            | Some "battery" -> Battery
            | Some "sensor" -> Sensor
            | Some "fire_detector" -> FireDetector
            | Some "leak_detector" -> LeakDetector
            | Some "meter" -> Meter
            | Some x -> Unknown x)

type Device =
    { Id: int
      Name: string
      Room: int option
      Model: string option
      ModelAlias: string option
      DeviceKind: DeviceKind
      DeviceParams: DeviceParams
      Services: Service list }

module Device =
    let decoder : Decoder<Device> =
        Decode.object (fun get ->
            { Id = get.Required.Field "id" Decode.int
              Name = get.Required.At [ "client"; "name" ] Decode.string
              Room = get.Optional.Field "room" Decode.int
              Model = get.Optional.Field "model" Decode.string
              ModelAlias = get.Optional.Field "modelAlias" Decode.string
              DeviceKind = get.Required.Field "type" DeviceKind.decoder
              DeviceParams = get.Required.Field "param" DeviceParams.decoder
              Services = get.Required.Field "services" Service.decoder })

    let devicesDecoder : Decoder<Device list> =
        Decode.object (fun get ->
            get.Required.At [ "val"; "param"; "device" ] (Decode.list decoder))

    let decodeAll json = Decode.fromString devicesDecoder json

type Room =
    { Name: string
      Area: int
      Id: int }

module Room =
    let decoder : Decoder<Room> =
        Decode.object (fun get ->
            { Name = get.Required.Field "alias" Decode.string
              Area = get.Required.Field "area" Decode.int
              Id = get.Required.Field "id" Decode.int })

    let roomsDecoder : Decoder<Room list> =
        Decode.object (fun get ->
            get.Required.At [ "val"; "param"; "room" ] (Decode.list decoder))

    let decodeAll json = Decode.fromString roomsDecoder json