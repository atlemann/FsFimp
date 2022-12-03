module FsFimp.Commands

open FsFimp.Fimp

module Units =
    type [<Measure>] Red
    type [<Measure>] Green
    type [<Measure>] Blue

    let int =
        LanguagePrimitives.Int32WithMeasure

    let single =
        LanguagePrimitives.Float32WithMeasure

type LevelSwitch = On | Off

module LevelSwitch =
    let interfaceType = Type.create "cmd.binary.set"
    let service = Serv.OutLevelSwitch

    let createMessage (toggle: LevelSwitch) =
        match toggle with
        | On -> true
        | Off -> false
        |> Val.Bool
        |> Message.createTimeStamped
            Props.empty
            service
            src
            interfaceType

type Red = int<Units.Red>
type Green = int<Units.Green>
type Blue = int<Units.Blue>

module Color =
    let interfaceType = Type.create "cmd.color.set"
    let service = Serv.ColorControl

    let createMessage (red: Red) (green: Green) (blue: Blue) =
        seq {
            "red", int red
            "green", int green
            "blue", int blue
        }
        |> Map.ofSeq
        |> Val.Int_map
        |> Message.createTimeStamped
            Props.empty
            service
            src
            interfaceType
