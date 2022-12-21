module FsFimp.Devices

open FsFimp.Fimp
open Thoth.Json.Net

// module FibaroMotionSensor =
//
//     type Alarm =
//         | Inactive
//         | TamperRemoveCover
//         static member Of value =
//             match value with
//             | "inactive" -> Inactive
//             | "tamper_removed_cover" -> TamperRemoveCover
//             | x -> failwith $"Unknown alarm {x}"
//
//     type Data =
//         { Id: int
//           Model: string
//           BatteryPercentage: int
//           Illuminance: single option
//           Presence: bool option
//           Alarm: Alarm option
//           Temperature: single }
//
//     let decode : Decoder<Data> =
//         Decode.object (fun get ->
//             let id = get.Required.Field "id" Decode.int
//             let model = get.Required.At [ "param"; "model" ] Decode.string
//             let batteryPercentage = get.Required.At [ "param"; "param"; "batteryPercentage" ] Decode.int
//             let illuminance = get.Optional.At [ "changes"; "lighting"; "illuminance" ] Decode.float32
//             let presence = get.Optional.At [ "changes"; "presence" ] Decode.bool
//             let alarm = get.Optional.At [ "changes"; "alarms"; "burglar" ] (Decode.list (Decode.string |> Decode.map Alarm.Of))
//             let temperature = get.Required.At [ "param"; "param"; "temperature" ] Decode.float32
//
//             { Id = id
//               Model = model
//               BatteryPercentage = batteryPercentage
//               Illuminance = illuminance
//               Presence = presence
//               Alarm = alarm |> Option.map List.head
//               Temperature = temperature })
//
//     let (|DeviceId|_|) deviceId =
//         if deviceId = "zw_271_2049_4097" then
//             Some decode
//         else
//             None
//
// type Device =
//     | FibaroMotionSensor of FibaroMotionSensor.Data
//     | Unknown of string
//
// module Device =
//
//     let nullableStringDecoder : Decoder<string> =
//         Decode.oneOf
//             [
//                 // First try to decode it as a standard int
//                 Decode.string
//                 // If it fails, try to decode it as a null
//                 Decode.nil ""
//             ]
//
//     let (|Device|Room|Unknown|) (get: Decode.IGetters) =
//         match get.Required.At [ "val"; "component" ] Decode.string with
//         | "device" -> Device
//         | "room" -> Room
//         | x -> Unknown x
//
//     let decoder : Decoder<Device option> =
//         Decode.object (fun get ->
//             match get with
//             | Device ->
//                 let model = get.Optional.At [ "val"; "param"; "model" ] Decode.string
//                 let modelAlias = get.Optional.At [ "val"; "param"; "modelAlias" ] Decode.string
//
//                 let modelName =
//                     match model, modelAlias with
//                     | Some model, _ -> Some model
//                     | _, Some alias -> Some alias
//                     | _, _ -> None
//
//                 let getVal = get.Required.Field "val"
//
//                 match modelName with
//                 | Some (FibaroMotionSensor.DeviceId decoder) ->
//                     decoder
//                     |> Decode.map (FibaroMotionSensor >> Some)
//                     |> getVal
//
//                 | x ->
//                     x |> Option.map Unknown
//             | _ ->
//                 None)
//
//     let decode json = Decode.fromString decoder json

type Service =
    | Battery of ResponseTopic
    | BurglarAlarm of ResponseTopic
    | Luminance of ResponseTopic
    | Presence of ResponseTopic
    | Temperature of ResponseTopic

module Service =
    let decode : Decoder<Service list> =
        Decode.object (fun get ->
            [
                get.Optional.At [ "battery"; "addr" ] Decode.string |> Option.map (ResponseTopic.createFromResourceType >> Battery)
                get.Optional.At [ "alarm_burglar"; "addr" ] Decode.string |> Option.map (ResponseTopic.createFromResourceType >> BurglarAlarm)
                get.Optional.At [ "sensor_lumin"; "addr" ] Decode.string |> Option.map (ResponseTopic.createFromResourceType >> Luminance)
                get.Optional.At [ "sensor_presence"; "addr" ] Decode.string |> Option.map (ResponseTopic.createFromResourceType >> Presence)
                get.Optional.At [ "sensor_temp"; "addr" ] Decode.string |> Option.map (ResponseTopic.createFromResourceType >> Temperature)
            ]
            |> List.choose id)

type Device =
    { Id: int
      Room: int option
      Model: string option
      Services: Service list }

module Device =
    let decoder : Decoder<Device> =
        Decode.object (fun get ->
            let id = get.Required.Field "id" Decode.int
            let room = get.Optional.Field "room" Decode.int

            let model = get.Optional.At [ "model" ] Decode.string
            let modelAlias = get.Optional.At [ "modelAlias" ] Decode.string

            let modelName =
                match model, modelAlias with
                | Some model, _ -> Some model
                | _, Some alias -> Some alias
                | _, _ -> None

            let services = get.Required.Field "services" Service.decode

            { Id = id
              Room = room
              Model = modelName
              Services = services })

    let devicesDecoder : Decoder<Device list> =
        Decode.object (fun get ->
            get.Required.At [ "val"; "param"; "device" ] (Decode.list decoder))

    let decodeAll json = Decode.fromString devicesDecoder json
