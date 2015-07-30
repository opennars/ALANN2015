module AkkaSystem

    open Akka.Actor
    open Akka.FSharp

    type AkkaSystem() =
        static member System = ActorSystem.Create("ALANN")
