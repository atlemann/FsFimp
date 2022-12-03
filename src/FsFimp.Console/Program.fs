open System
open System.Threading.Tasks
open FsFimp
open FsFimp.Commands
open FsFimp.Fimp
open FsFimp.Queries
open FsFimp.MqttClient
open MQTTnet.Extensions.External.RxMQTT.Client
open FSharp.Control.Reactive
open System.Reactive.Threading.Tasks

let getAsync (mqttClient: IRxMqttClient) requestTopic responseTopic message = task {
    let respObs =
        responseTopic
        |> MqttClient.createSubscription mqttClient
        // We want the first message returned and the observable to complete
        |> Observable.first

    let response = respObs.ToTask()

    do! message
        |> MqttClient.createMessage requestTopic
        |> mqttClient.PublishAsync

    return! response
    }

/// Gets all available devices
let getAllDevices (mqttClient: IRxMqttClient) = task {
    let! response =
        Things.listDevices
        |> Things.createMessage
        |> getAsync mqttClient Things.requestTopic Things.responseTopic
    let devices =
        response.ApplicationMessage.Payload.ToUTF8String()
        |> Devices.Device.decodeAll
    return devices
    }

let test server credentials = task {
    use! mqttClient = MqttClient.create (ClientId "FSmartDash") server credentials
    use _ = mqttClient.Connected.Subscribe(fun isConnected -> printfn $"IsConnected: {isConnected}")
    use _ = mqttClient.ConnectingFailedEvent.Subscribe(fun msg -> printfn "%A, %A" msg.ConnectResult msg.Exception.Message)
    let send (msg: MQTTnet.MqttApplicationMessage) : Task<unit> =
        task {
            return! mqttClient.PublishAsync msg
        }

    let turnOnLightOnPresence =
        "pt:j1/mt:evt/rt:dev/rn:zw/ad:1/sv:sensor_presence/ad:19_0"
        |> ResponseTopic.create
        |> MqttClient.createSubscription mqttClient
        |> Observable.map (fun msg ->
            let requestTopic =
                "pt:j1/mt:cmd/rt:dev/rn:hue/ad:1/sv:out_lvl_switch/ad:l14_0"
                |> RequestTopic.create

            LevelSwitch.createMessage LevelSwitch.On
            |> MqttClient.createMessage requestTopic)

    let makeLightRedOnBurglar =
        "pt:j1/mt:evt/rt:dev/rn:zw/ad:1/sv:alarm_burglar/ad:19_0"
        |> ResponseTopic.create
        |> MqttClient.createSubscription mqttClient
        |> Observable.map (fun msg ->
            let requestTopic =
                "pt:j1/mt:cmd/rt:dev/rn:hue/ad:1/sv:color_ctrl/ad:l14_0"
                |> RequestTopic.create

            Color.createMessage 255<Units.Red> 0<Units.Green> 0<Units.Blue>
            |> MqttClient.createMessage requestTopic)

    use _ =
        Observable.merge turnOnLightOnPresence makeLightRedOnBurglar
        |> Observable.flatmapTask send
        |> Observable.subscribe id

    // do! Task.Delay 5000
    printfn "Press any key to exit"
    System.Console.ReadKey() |> ignore
}

let tcpServer = { Url = "<url>"; Port = 1884 }
let cred = { UserName = "<username>"; Password = "<password>" }

test tcpServer cred
|> Async.AwaitTask
|> Async.RunSynchronously