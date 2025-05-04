module FsFimp.Commands

open FsFimp.Fimp
open FsFimp.Units
open FsFimp.Devices

type LevelSwitch = On | Off

module LevelSwitch =
    let interfaceType = Type.create "cmd.binary.set"
    let service = Serv.OutBinSwitch

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

type Color =
    { Red: int<Red>
      Green: int<Green>
      Blue: int<Blue> }

module Color =
    let interfaceType = Interface.CmdColorSet
    let service = Serv.ColorControl

    let createMessage (color: Color) =
        seq {
            "red", int color.Red
            "green", int color.Green
            "blue", int color.Blue
        }
        |> Map.ofSeq
        |> Val.Int_map
        |> Message.createTimeStamped
            Props.empty
            service
            src
            (interfaceType |> Interface.toString |> Type.create)

type Thermostat =
    { Temperature: float<Temperature> }

module Thermostat =
    let interfaceType = Interface.CmdSetpointSet
    let service = Serv.Thermostat

    let createMessage (thermostat: Thermostat) =
        Val.Float (float thermostat.Temperature)
        |> Message.createTimeStamped
            Props.empty
            service
            src
            (interfaceType |> Interface.toString |> Type.create)

type Dimmer =
    { Level: float<Percentage> }

module Dimmer =
    let interfaceType = Interface.CmdLevelSet
    let service = Serv.OutLevelSwitch

    let createMessage (dimmer: Dimmer) =
        Val.Float (float dimmer.Level)
        |> Message.createTimeStamped
            Props.empty
            service
            src
            (interfaceType |> Interface.toString |> Type.create)

module PowerMeter =
    let interfaceType = Interface.CmdMeterGetReport
    let service = Serv.PowerMeter

    let createMessage () =
        Val.Null
        |> Message.createTimeStamped
            Props.empty
            service
            src
            (interfaceType |> Interface.toString |> Type.create)
