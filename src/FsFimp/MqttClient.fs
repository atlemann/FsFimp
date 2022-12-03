module FsFimp.MqttClient

open FsFimp.Fimp
open MQTTnet
open MQTTnet.Client
open MQTTnet.Extensions.External.RxMQTT.Client
open MQTTnet.Extensions.ManagedClient
open System
open MQTTnet.Formatter
open Thoth.Json.Net

type ClientId = ClientId of string

type TcpServer =
    { Url: string
      Port: int }

type Credentials =
    { UserName: string
      Password: string }

let create (ClientId clientId) (server: TcpServer) (credentials: Credentials) = task {
    // Setup and start a rx MQTT client.
    let options =
        ManagedMqttClientOptionsBuilder()
            .WithAutoReconnectDelay(TimeSpan.FromSeconds(5))
            .WithClientOptions(MqttClientOptionsBuilder()
                .WithProtocolVersion(MqttProtocolVersion.V311)
                .WithClientId(clientId)
                .WithTcpServer(server.Url, server.Port)
                .WithCredentials(credentials.UserName, credentials.Password)
                .Build())
            .Build();

    let mqttClient = MqttFactory().CreateRxMqttClient()
    do! mqttClient.StartAsync options
    return mqttClient
}

/// Subscribes to the given topic as an observable
let createSubscription (mqttClient: IRxMqttClient) (topic: ResponseTopic) =
    mqttClient.Connect(topic |> ResponseTopic.value)

let createMessage (topic: RequestTopic) (message: Message) =
    MqttApplicationMessageBuilder()
        .WithTopic(topic |> RequestTopic.value)
        .WithPayload(message |> Message.encode |> Encode.toString 0)
        .WithQualityOfServiceLevel(MQTTnet.Protocol.MqttQualityOfServiceLevel.ExactlyOnce)
        .WithRetainFlag()
        .Build()